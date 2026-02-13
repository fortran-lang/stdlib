
submodule(stdlib_sparse_kinds) stdlib_sparse_operators
  implicit none

  contains

    pure module function sparse_add_COO_sp(a, b) result(c)
      type(COO_sp_type), intent(in) :: a, b
      type(COO_sp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_COO_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(COO_sp_type), intent(in) :: b
      type(COO_sp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_COO_sp(a, b) result(c)
      type(COO_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(COO_sp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_COO_dp(a, b) result(c)
      type(COO_dp_type), intent(in) :: a, b
      type(COO_dp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_COO_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(COO_dp_type), intent(in) :: b
      type(COO_dp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_COO_dp(a, b) result(c)
      type(COO_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(COO_dp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_COO_csp(a, b) result(c)
      type(COO_csp_type), intent(in) :: a, b
      type(COO_csp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_COO_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(COO_csp_type), intent(in) :: b
      type(COO_csp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_COO_csp(a, b) result(c)
      type(COO_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(COO_csp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_COO_cdp(a, b) result(c)
      type(COO_cdp_type), intent(in) :: a, b
      type(COO_cdp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_COO_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(COO_cdp_type), intent(in) :: b
      type(COO_cdp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_COO_cdp(a, b) result(c)
      type(COO_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(COO_cdp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_CSR_sp(a, b) result(c)
      type(CSR_sp_type), intent(in) :: a, b
      type(CSR_sp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_CSR_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(CSR_sp_type), intent(in) :: b
      type(CSR_sp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_CSR_sp(a, b) result(c)
      type(CSR_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(CSR_sp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_CSR_dp(a, b) result(c)
      type(CSR_dp_type), intent(in) :: a, b
      type(CSR_dp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_CSR_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(CSR_dp_type), intent(in) :: b
      type(CSR_dp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_CSR_dp(a, b) result(c)
      type(CSR_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(CSR_dp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_CSR_csp(a, b) result(c)
      type(CSR_csp_type), intent(in) :: a, b
      type(CSR_csp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_CSR_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(CSR_csp_type), intent(in) :: b
      type(CSR_csp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_CSR_csp(a, b) result(c)
      type(CSR_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(CSR_csp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_CSR_cdp(a, b) result(c)
      type(CSR_cdp_type), intent(in) :: a, b
      type(CSR_cdp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_CSR_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(CSR_cdp_type), intent(in) :: b
      type(CSR_cdp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_CSR_cdp(a, b) result(c)
      type(CSR_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(CSR_cdp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_CSC_sp(a, b) result(c)
      type(CSC_sp_type), intent(in) :: a, b
      type(CSC_sp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_CSC_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(CSC_sp_type), intent(in) :: b
      type(CSC_sp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_CSC_sp(a, b) result(c)
      type(CSC_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(CSC_sp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_CSC_dp(a, b) result(c)
      type(CSC_dp_type), intent(in) :: a, b
      type(CSC_dp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_CSC_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(CSC_dp_type), intent(in) :: b
      type(CSC_dp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_CSC_dp(a, b) result(c)
      type(CSC_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(CSC_dp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_CSC_csp(a, b) result(c)
      type(CSC_csp_type), intent(in) :: a, b
      type(CSC_csp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_CSC_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(CSC_csp_type), intent(in) :: b
      type(CSC_csp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_CSC_csp(a, b) result(c)
      type(CSC_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(CSC_csp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_CSC_cdp(a, b) result(c)
      type(CSC_cdp_type), intent(in) :: a, b
      type(CSC_cdp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_CSC_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(CSC_cdp_type), intent(in) :: b
      type(CSC_cdp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_CSC_cdp(a, b) result(c)
      type(CSC_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(CSC_cdp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_ELL_sp(a, b) result(c)
      type(ELL_sp_type), intent(in) :: a, b
      type(ELL_sp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_ELL_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(ELL_sp_type), intent(in) :: b
      type(ELL_sp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_ELL_sp(a, b) result(c)
      type(ELL_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(ELL_sp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_ELL_dp(a, b) result(c)
      type(ELL_dp_type), intent(in) :: a, b
      type(ELL_dp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_ELL_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(ELL_dp_type), intent(in) :: b
      type(ELL_dp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_ELL_dp(a, b) result(c)
      type(ELL_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(ELL_dp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_ELL_csp(a, b) result(c)
      type(ELL_csp_type), intent(in) :: a, b
      type(ELL_csp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_ELL_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(ELL_csp_type), intent(in) :: b
      type(ELL_csp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_ELL_csp(a, b) result(c)
      type(ELL_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(ELL_csp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_add_ELL_cdp(a, b) result(c)
      type(ELL_cdp_type), intent(in) :: a, b
      type(ELL_cdp_type) :: c
      c = a
      c%data = c%data + b%data
    end function

    pure module function sparse_add_ELL_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(ELL_cdp_type), intent(in) :: b
      type(ELL_cdp_type) :: c
      c = b
      c%data = a + c%data
    end function

    pure module function sparse_add_scalar_ELL_cdp(a, b) result(c)
      type(ELL_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(ELL_cdp_type) :: c
      c = a
      c%data = c%data + b
    end function

    pure module function sparse_sub_COO_sp(a, b) result(c)
      type(COO_sp_type), intent(in) :: a, b
      type(COO_sp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_COO_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(COO_sp_type), intent(in) :: b
      type(COO_sp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_COO_sp(a, b) result(c)
      type(COO_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(COO_sp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_COO_dp(a, b) result(c)
      type(COO_dp_type), intent(in) :: a, b
      type(COO_dp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_COO_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(COO_dp_type), intent(in) :: b
      type(COO_dp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_COO_dp(a, b) result(c)
      type(COO_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(COO_dp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_COO_csp(a, b) result(c)
      type(COO_csp_type), intent(in) :: a, b
      type(COO_csp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_COO_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(COO_csp_type), intent(in) :: b
      type(COO_csp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_COO_csp(a, b) result(c)
      type(COO_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(COO_csp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_COO_cdp(a, b) result(c)
      type(COO_cdp_type), intent(in) :: a, b
      type(COO_cdp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_COO_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(COO_cdp_type), intent(in) :: b
      type(COO_cdp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_COO_cdp(a, b) result(c)
      type(COO_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(COO_cdp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_CSR_sp(a, b) result(c)
      type(CSR_sp_type), intent(in) :: a, b
      type(CSR_sp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_CSR_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(CSR_sp_type), intent(in) :: b
      type(CSR_sp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_CSR_sp(a, b) result(c)
      type(CSR_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(CSR_sp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_CSR_dp(a, b) result(c)
      type(CSR_dp_type), intent(in) :: a, b
      type(CSR_dp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_CSR_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(CSR_dp_type), intent(in) :: b
      type(CSR_dp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_CSR_dp(a, b) result(c)
      type(CSR_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(CSR_dp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_CSR_csp(a, b) result(c)
      type(CSR_csp_type), intent(in) :: a, b
      type(CSR_csp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_CSR_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(CSR_csp_type), intent(in) :: b
      type(CSR_csp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_CSR_csp(a, b) result(c)
      type(CSR_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(CSR_csp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_CSR_cdp(a, b) result(c)
      type(CSR_cdp_type), intent(in) :: a, b
      type(CSR_cdp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_CSR_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(CSR_cdp_type), intent(in) :: b
      type(CSR_cdp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_CSR_cdp(a, b) result(c)
      type(CSR_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(CSR_cdp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_CSC_sp(a, b) result(c)
      type(CSC_sp_type), intent(in) :: a, b
      type(CSC_sp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_CSC_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(CSC_sp_type), intent(in) :: b
      type(CSC_sp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_CSC_sp(a, b) result(c)
      type(CSC_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(CSC_sp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_CSC_dp(a, b) result(c)
      type(CSC_dp_type), intent(in) :: a, b
      type(CSC_dp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_CSC_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(CSC_dp_type), intent(in) :: b
      type(CSC_dp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_CSC_dp(a, b) result(c)
      type(CSC_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(CSC_dp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_CSC_csp(a, b) result(c)
      type(CSC_csp_type), intent(in) :: a, b
      type(CSC_csp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_CSC_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(CSC_csp_type), intent(in) :: b
      type(CSC_csp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_CSC_csp(a, b) result(c)
      type(CSC_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(CSC_csp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_CSC_cdp(a, b) result(c)
      type(CSC_cdp_type), intent(in) :: a, b
      type(CSC_cdp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_CSC_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(CSC_cdp_type), intent(in) :: b
      type(CSC_cdp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_CSC_cdp(a, b) result(c)
      type(CSC_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(CSC_cdp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_ELL_sp(a, b) result(c)
      type(ELL_sp_type), intent(in) :: a, b
      type(ELL_sp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_ELL_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(ELL_sp_type), intent(in) :: b
      type(ELL_sp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_ELL_sp(a, b) result(c)
      type(ELL_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(ELL_sp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_ELL_dp(a, b) result(c)
      type(ELL_dp_type), intent(in) :: a, b
      type(ELL_dp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_ELL_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(ELL_dp_type), intent(in) :: b
      type(ELL_dp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_ELL_dp(a, b) result(c)
      type(ELL_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(ELL_dp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_ELL_csp(a, b) result(c)
      type(ELL_csp_type), intent(in) :: a, b
      type(ELL_csp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_ELL_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(ELL_csp_type), intent(in) :: b
      type(ELL_csp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_ELL_csp(a, b) result(c)
      type(ELL_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(ELL_csp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_sub_ELL_cdp(a, b) result(c)
      type(ELL_cdp_type), intent(in) :: a, b
      type(ELL_cdp_type) :: c
      c = a
      c%data = c%data - b%data
    end function

    pure module function sparse_sub_ELL_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(ELL_cdp_type), intent(in) :: b
      type(ELL_cdp_type) :: c
      c = b
      c%data = a - c%data
    end function

    pure module function sparse_sub_scalar_ELL_cdp(a, b) result(c)
      type(ELL_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(ELL_cdp_type) :: c
      c = a
      c%data = c%data - b
    end function

    pure module function sparse_mul_COO_sp(a, b) result(c)
      type(COO_sp_type), intent(in) :: a, b
      type(COO_sp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_COO_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(COO_sp_type), intent(in) :: b
      type(COO_sp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_COO_sp(a, b) result(c)
      type(COO_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(COO_sp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_COO_dp(a, b) result(c)
      type(COO_dp_type), intent(in) :: a, b
      type(COO_dp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_COO_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(COO_dp_type), intent(in) :: b
      type(COO_dp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_COO_dp(a, b) result(c)
      type(COO_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(COO_dp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_COO_csp(a, b) result(c)
      type(COO_csp_type), intent(in) :: a, b
      type(COO_csp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_COO_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(COO_csp_type), intent(in) :: b
      type(COO_csp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_COO_csp(a, b) result(c)
      type(COO_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(COO_csp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_COO_cdp(a, b) result(c)
      type(COO_cdp_type), intent(in) :: a, b
      type(COO_cdp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_COO_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(COO_cdp_type), intent(in) :: b
      type(COO_cdp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_COO_cdp(a, b) result(c)
      type(COO_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(COO_cdp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_CSR_sp(a, b) result(c)
      type(CSR_sp_type), intent(in) :: a, b
      type(CSR_sp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_CSR_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(CSR_sp_type), intent(in) :: b
      type(CSR_sp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_CSR_sp(a, b) result(c)
      type(CSR_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(CSR_sp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_CSR_dp(a, b) result(c)
      type(CSR_dp_type), intent(in) :: a, b
      type(CSR_dp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_CSR_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(CSR_dp_type), intent(in) :: b
      type(CSR_dp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_CSR_dp(a, b) result(c)
      type(CSR_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(CSR_dp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_CSR_csp(a, b) result(c)
      type(CSR_csp_type), intent(in) :: a, b
      type(CSR_csp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_CSR_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(CSR_csp_type), intent(in) :: b
      type(CSR_csp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_CSR_csp(a, b) result(c)
      type(CSR_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(CSR_csp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_CSR_cdp(a, b) result(c)
      type(CSR_cdp_type), intent(in) :: a, b
      type(CSR_cdp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_CSR_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(CSR_cdp_type), intent(in) :: b
      type(CSR_cdp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_CSR_cdp(a, b) result(c)
      type(CSR_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(CSR_cdp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_CSC_sp(a, b) result(c)
      type(CSC_sp_type), intent(in) :: a, b
      type(CSC_sp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_CSC_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(CSC_sp_type), intent(in) :: b
      type(CSC_sp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_CSC_sp(a, b) result(c)
      type(CSC_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(CSC_sp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_CSC_dp(a, b) result(c)
      type(CSC_dp_type), intent(in) :: a, b
      type(CSC_dp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_CSC_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(CSC_dp_type), intent(in) :: b
      type(CSC_dp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_CSC_dp(a, b) result(c)
      type(CSC_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(CSC_dp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_CSC_csp(a, b) result(c)
      type(CSC_csp_type), intent(in) :: a, b
      type(CSC_csp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_CSC_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(CSC_csp_type), intent(in) :: b
      type(CSC_csp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_CSC_csp(a, b) result(c)
      type(CSC_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(CSC_csp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_CSC_cdp(a, b) result(c)
      type(CSC_cdp_type), intent(in) :: a, b
      type(CSC_cdp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_CSC_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(CSC_cdp_type), intent(in) :: b
      type(CSC_cdp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_CSC_cdp(a, b) result(c)
      type(CSC_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(CSC_cdp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_ELL_sp(a, b) result(c)
      type(ELL_sp_type), intent(in) :: a, b
      type(ELL_sp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_ELL_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(ELL_sp_type), intent(in) :: b
      type(ELL_sp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_ELL_sp(a, b) result(c)
      type(ELL_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(ELL_sp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_ELL_dp(a, b) result(c)
      type(ELL_dp_type), intent(in) :: a, b
      type(ELL_dp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_ELL_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(ELL_dp_type), intent(in) :: b
      type(ELL_dp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_ELL_dp(a, b) result(c)
      type(ELL_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(ELL_dp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_ELL_csp(a, b) result(c)
      type(ELL_csp_type), intent(in) :: a, b
      type(ELL_csp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_ELL_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(ELL_csp_type), intent(in) :: b
      type(ELL_csp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_ELL_csp(a, b) result(c)
      type(ELL_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(ELL_csp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_mul_ELL_cdp(a, b) result(c)
      type(ELL_cdp_type), intent(in) :: a, b
      type(ELL_cdp_type) :: c
      c = a
      c%data = c%data * b%data
    end function

    pure module function sparse_mul_ELL_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(ELL_cdp_type), intent(in) :: b
      type(ELL_cdp_type) :: c
      c = b
      c%data = a * c%data
    end function

    pure module function sparse_mul_scalar_ELL_cdp(a, b) result(c)
      type(ELL_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(ELL_cdp_type) :: c
      c = a
      c%data = c%data * b
    end function

    pure module function sparse_div_COO_sp(a, b) result(c)
      type(COO_sp_type), intent(in) :: a, b
      type(COO_sp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_COO_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(COO_sp_type), intent(in) :: b
      type(COO_sp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_COO_sp(a, b) result(c)
      type(COO_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(COO_sp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_COO_dp(a, b) result(c)
      type(COO_dp_type), intent(in) :: a, b
      type(COO_dp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_COO_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(COO_dp_type), intent(in) :: b
      type(COO_dp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_COO_dp(a, b) result(c)
      type(COO_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(COO_dp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_COO_csp(a, b) result(c)
      type(COO_csp_type), intent(in) :: a, b
      type(COO_csp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_COO_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(COO_csp_type), intent(in) :: b
      type(COO_csp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_COO_csp(a, b) result(c)
      type(COO_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(COO_csp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_COO_cdp(a, b) result(c)
      type(COO_cdp_type), intent(in) :: a, b
      type(COO_cdp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_COO_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(COO_cdp_type), intent(in) :: b
      type(COO_cdp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_COO_cdp(a, b) result(c)
      type(COO_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(COO_cdp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_CSR_sp(a, b) result(c)
      type(CSR_sp_type), intent(in) :: a, b
      type(CSR_sp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_CSR_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(CSR_sp_type), intent(in) :: b
      type(CSR_sp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_CSR_sp(a, b) result(c)
      type(CSR_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(CSR_sp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_CSR_dp(a, b) result(c)
      type(CSR_dp_type), intent(in) :: a, b
      type(CSR_dp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_CSR_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(CSR_dp_type), intent(in) :: b
      type(CSR_dp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_CSR_dp(a, b) result(c)
      type(CSR_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(CSR_dp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_CSR_csp(a, b) result(c)
      type(CSR_csp_type), intent(in) :: a, b
      type(CSR_csp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_CSR_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(CSR_csp_type), intent(in) :: b
      type(CSR_csp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_CSR_csp(a, b) result(c)
      type(CSR_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(CSR_csp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_CSR_cdp(a, b) result(c)
      type(CSR_cdp_type), intent(in) :: a, b
      type(CSR_cdp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_CSR_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(CSR_cdp_type), intent(in) :: b
      type(CSR_cdp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_CSR_cdp(a, b) result(c)
      type(CSR_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(CSR_cdp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_CSC_sp(a, b) result(c)
      type(CSC_sp_type), intent(in) :: a, b
      type(CSC_sp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_CSC_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(CSC_sp_type), intent(in) :: b
      type(CSC_sp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_CSC_sp(a, b) result(c)
      type(CSC_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(CSC_sp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_CSC_dp(a, b) result(c)
      type(CSC_dp_type), intent(in) :: a, b
      type(CSC_dp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_CSC_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(CSC_dp_type), intent(in) :: b
      type(CSC_dp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_CSC_dp(a, b) result(c)
      type(CSC_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(CSC_dp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_CSC_csp(a, b) result(c)
      type(CSC_csp_type), intent(in) :: a, b
      type(CSC_csp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_CSC_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(CSC_csp_type), intent(in) :: b
      type(CSC_csp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_CSC_csp(a, b) result(c)
      type(CSC_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(CSC_csp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_CSC_cdp(a, b) result(c)
      type(CSC_cdp_type), intent(in) :: a, b
      type(CSC_cdp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_CSC_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(CSC_cdp_type), intent(in) :: b
      type(CSC_cdp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_CSC_cdp(a, b) result(c)
      type(CSC_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(CSC_cdp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_ELL_sp(a, b) result(c)
      type(ELL_sp_type), intent(in) :: a, b
      type(ELL_sp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_ELL_scalar_sp(a, b) result(c)
      real(sp), intent(in) :: a
      type(ELL_sp_type), intent(in) :: b
      type(ELL_sp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_ELL_sp(a, b) result(c)
      type(ELL_sp_type), intent(in) :: a
      real(sp), intent(in) :: b
      type(ELL_sp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_ELL_dp(a, b) result(c)
      type(ELL_dp_type), intent(in) :: a, b
      type(ELL_dp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_ELL_scalar_dp(a, b) result(c)
      real(dp), intent(in) :: a
      type(ELL_dp_type), intent(in) :: b
      type(ELL_dp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_ELL_dp(a, b) result(c)
      type(ELL_dp_type), intent(in) :: a
      real(dp), intent(in) :: b
      type(ELL_dp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_ELL_csp(a, b) result(c)
      type(ELL_csp_type), intent(in) :: a, b
      type(ELL_csp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_ELL_scalar_csp(a, b) result(c)
      complex(sp), intent(in) :: a
      type(ELL_csp_type), intent(in) :: b
      type(ELL_csp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_ELL_csp(a, b) result(c)
      type(ELL_csp_type), intent(in) :: a
      complex(sp), intent(in) :: b
      type(ELL_csp_type) :: c
      c = a
      c%data = c%data / b
    end function

    pure module function sparse_div_ELL_cdp(a, b) result(c)
      type(ELL_cdp_type), intent(in) :: a, b
      type(ELL_cdp_type) :: c
      c = a
      c%data = c%data / b%data
    end function

    pure module function sparse_div_ELL_scalar_cdp(a, b) result(c)
      complex(dp), intent(in) :: a
      type(ELL_cdp_type), intent(in) :: b
      type(ELL_cdp_type) :: c
      c = b
      c%data = a / c%data
    end function

    pure module function sparse_div_scalar_ELL_cdp(a, b) result(c)
      type(ELL_cdp_type), intent(in) :: a
      complex(dp), intent(in) :: b
      type(ELL_cdp_type) :: c
      c = a
      c%data = c%data / b
    end function


end submodule stdlib_sparse_operators