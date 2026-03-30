
submodule (stdlib_math) stdlib_math_logspace

implicit none

contains

    module procedure logspace_1_rsp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, sp))
    end procedure
    module procedure logspace_1_rdp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, dp))
    end procedure
    module procedure logspace_1_csp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, sp))
    end procedure
    module procedure logspace_1_cdp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, dp))
    end procedure
    module procedure logspace_1_iint32_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, DEFAULT_LOGSPACE_BASE)
    end procedure

    module procedure logspace_1_rsp_n
      res = logspace(start, end, n, real(DEFAULT_LOGSPACE_BASE, sp))
    end procedure
    module procedure logspace_1_rdp_n
      res = logspace(start, end, n, real(DEFAULT_LOGSPACE_BASE, dp))
    end procedure
    module procedure logspace_1_csp_n
      res = logspace(start, end, n, real(DEFAULT_LOGSPACE_BASE, sp))
    end procedure
    module procedure logspace_1_cdp_n
      res = logspace(start, end, n, real(DEFAULT_LOGSPACE_BASE, dp))
    end procedure
    module procedure logspace_1_iint32_n
      res = logspace(start, end, n, DEFAULT_LOGSPACE_BASE)
    end procedure

    module procedure logspace_1_rsp_n_rbase
      real(sp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_rsp_n_cbase
      real(sp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_rsp_n_ibase
      real(sp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure
    module procedure logspace_1_rdp_n_rbase
      real(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_rdp_n_cbase
      real(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_rdp_n_ibase
      real(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure
    module procedure logspace_1_csp_n_rbase
      complex(sp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_csp_n_cbase
      complex(sp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_csp_n_ibase
      complex(sp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure
    module procedure logspace_1_cdp_n_rbase
      complex(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_cdp_n_cbase
      complex(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_cdp_n_ibase
      complex(dp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure
    ! Generate logarithmically spaced sequence from dp base to the powers
    ! of dp start and end. [base^start, ... , base^end]
    ! RName = logspace_1_cdp_n_ibase
    module procedure logspace_1_iint32_n_rspbase
      real(sp) :: exponents(max(n, 0))
      exponents = linspace(real(start, kind=sp), real(end, kind=sp), n)
      res = base ** exponents
      if (n > 1) res(1) = base ** start
      if (n > 0) res(n) = base ** end
    end procedure

    module procedure logspace_1_iint32_n_cspbase
      complex(sp) :: exponents(max(n, 0))
      exponents = cmplx(linspace(real(start, kind=sp), real(end, kind=sp), n), 0.0_sp)
      res = base ** exponents
      if (n > 1) res(1) = base ** start
      if (n > 0) res(n) = base ** end
    end procedure
    module procedure logspace_1_iint32_n_rdpbase
      real(dp) :: exponents(max(n, 0))
      exponents = linspace(real(start, kind=dp), real(end, kind=dp), n)
      res = base ** exponents
      if (n > 1) res(1) = base ** start
      if (n > 0) res(n) = base ** end
    end procedure

    module procedure logspace_1_iint32_n_cdpbase
      complex(dp) :: exponents(max(n, 0))
      exponents = cmplx(linspace(real(start, kind=dp), real(end, kind=dp), n), 0.0_dp)
      res = base ** exponents
      if (n > 1) res(1) = base ** start
      if (n > 0) res(n) = base ** end
    end procedure

    module procedure logspace_1_iint32_n_ibase
      real(dp) :: exponents(max(n, 0))
      exponents = linspace(real(start, kind=dp), real(end, kind=dp), n)
      res = base ** exponents
      if (n > 1) res(1) = base ** real(start, kind=dp)
      if (n > 0) res(n) = base ** real(end, kind=dp)
    end procedure


end submodule
