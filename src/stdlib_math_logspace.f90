
submodule (stdlib_math) stdlib_math_logspace

implicit none

contains

    module procedure logspace_1_rsp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, sp))
    end procedure
    module procedure logspace_1_rdp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, dp))
    end procedure
    module procedure logspace_1_rqp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, qp))
    end procedure
    module procedure logspace_1_csp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, sp))
    end procedure
    module procedure logspace_1_cdp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, dp))
    end procedure
    module procedure logspace_1_cqp_default
      res = logspace(start, end, DEFAULT_LOGSPACE_LENGTH, real(DEFAULT_LOGSPACE_BASE, qp))
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
    module procedure logspace_1_rqp_n
      res = logspace(start, end, n, real(DEFAULT_LOGSPACE_BASE, qp))
    end procedure
    module procedure logspace_1_csp_n
      res = logspace(start, end, n, real(DEFAULT_LOGSPACE_BASE, sp))
    end procedure
    module procedure logspace_1_cdp_n
      res = logspace(start, end, n, real(DEFAULT_LOGSPACE_BASE, dp))
    end procedure
    module procedure logspace_1_cqp_n
      res = logspace(start, end, n, real(DEFAULT_LOGSPACE_BASE, qp))
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
    module procedure logspace_1_rqp_n_rbase
      real(qp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_rqp_n_cbase
      real(qp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_rqp_n_ibase
      real(qp) :: exponents(max(n, 0))
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
    module procedure logspace_1_cqp_n_rbase
      complex(qp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_cqp_n_cbase
      complex(qp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_cqp_n_ibase
      complex(qp) :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure
    ! Generate logarithmically spaced sequence from qp base to the powers
    ! of qp start and end. [base^start, ... , base^end]
    ! RName = logspace_1_cqp_n_ibase
    module procedure logspace_1_iint32_n_rspbase
      integer :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_iint32_n_cspbase
      integer :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure
    module procedure logspace_1_iint32_n_rdpbase
      integer :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_iint32_n_cdpbase
      integer :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure
    module procedure logspace_1_iint32_n_rqpbase
      integer :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_iint32_n_cqpbase
      integer :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure

    module procedure logspace_1_iint32_n_ibase
      integer :: exponents(max(n, 0))
      exponents = linspace(start, end, n)
      res = base ** exponents
    end procedure


end submodule
