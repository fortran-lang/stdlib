
program test_distribution_beta
    use stdlib_kinds, only : sp, dp, xdp
    use stdlib_error, only : check
    use stdlib_random, only : random_seed
    use stdlib_stats_distribution_beta, only : rbeta => rvs_beta,           &
                              beta_pdf => pdf_beta, beta_cdf => cdf_beta

    implicit none
    real(sp), parameter :: sptol = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1.0_dp)
    logical :: warn = .true.
    integer :: put, get

    put = 12345678
    call random_seed(put, get)

    call test_beta_random_generator

    call test_beta_rvs_rsp
    call test_beta_rvs_rdp
    call test_beta_rvs_csp
    call test_beta_rvs_cdp

    call test_beta_pdf_rsp
    call test_beta_pdf_rdp
    call test_beta_pdf_csp
    call test_beta_pdf_cdp

    call test_beta_cdf_rsp
    call test_beta_cdf_rdp
    call test_beta_cdf_csp
    call test_beta_cdf_cdp

contains

    subroutine test_beta_random_generator
        integer, parameter :: num = 10000000, array_size = 1000
        integer :: i, j, freq(0:array_size-1)
        real(dp) :: chisq, expct

        print *, ""
        print *, "Test beta random generator with chi-squared"

        freq = 0
        do i = 1, num
            j = min(array_size - 1, int(array_size * beta_cdf(rbeta(2.0_dp, 5.0_dp, 0.0_dp), 2.0_dp, 5.0_dp, 0.0_dp)))
            freq(j) = freq(j) + 1
        end do

        chisq = 0.0_dp
        expct = num / array_size

        do i = 0, array_size - 1
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do

        write(*,*) "The critical values for chi-squared with 1000 d. of f. is" &
            //" 1143.92"
        write(*,*) "Chi-squared for beta random generator is : ", chisq
        call check((chisq < 1143.9), &
            msg="beta randomness failed chi-squared test", warn=warn)

    end subroutine test_beta_random_generator

    subroutine test_beta_rvs_rsp
        integer, parameter :: k = 5, n = 10
        real(sp) :: res(n), ans(n), ba, bb, loc
        integer :: i
        integer :: seed, get

        print *, "Test beta_distribution_rvs_rsp"
        seed = 639741825
        call random_seed(seed, get)

        ba = 2.0_sp; bb = 5.0_sp; loc = 0._sp

        do i = 1, k
            res(i) = rbeta(ba, bb, loc)
        end do
        res(k + 1 : n) = rbeta(ba, bb, k, loc)

        seed = 639741825
        call random_seed(seed, get)
        do i = 1, n
            ans(i) = rbeta(ba, bb, loc)
        end do

        call check(all(abs(res - ans) < sptol),                           &
                   msg="beta_distribution_rvs_rsp failed",      &
                   warn=warn)
    end subroutine test_beta_rvs_rsp

    subroutine test_beta_rvs_rdp
        integer, parameter :: k = 5, n = 10
        real(dp) :: res(n), ans(n), ba, bb, loc
        integer :: i
        integer :: seed, get

        print *, "Test beta_distribution_rvs_rdp"
        seed = 639741825
        call random_seed(seed, get)

        ba = 2.0_dp; bb = 5.0_dp; loc = 0._dp

        do i = 1, k
            res(i) = rbeta(ba, bb, loc)
        end do
        res(k + 1 : n) = rbeta(ba, bb, k, loc)

        seed = 639741825
        call random_seed(seed, get)
        do i = 1, n
            ans(i) = rbeta(ba, bb, loc)
        end do

        call check(all(abs(res - ans) < dptol),                           &
                   msg="beta_distribution_rvs_rdp failed",      &
                   warn=warn)
    end subroutine test_beta_rvs_rdp

    subroutine test_beta_rvs_csp
        integer, parameter :: k = 5, n = 10
        complex(sp) :: res(n), ans(n), ba, bb, loc
        integer :: i
        integer :: seed, get

        print *, "Test beta_distribution_rvs_csp"
        seed = 639741825
        call random_seed(seed, get)

        ba = (2.0_sp, 0.7_sp); bb = (5.0_sp, 3.0_sp); loc = (0._sp, 0._sp)

        do i = 1, k
            res(i) = rbeta(ba, bb, loc)
        end do
        res(k + 1 : n) = rbeta(ba, bb, k, loc)

        seed = 639741825
        call random_seed(seed, get)
        do i = 1, n
            ans(i) = rbeta(ba, bb, loc)
        end do

        call check(all(abs(res - ans) < sptol),                           &
                   msg="beta_distribution_rvs_csp failed",      &
                   warn=warn)
    end subroutine test_beta_rvs_csp

    subroutine test_beta_rvs_cdp
        integer, parameter :: k = 5, n = 10
        complex(dp) :: res(n), ans(n), ba, bb, loc
        integer :: i
        integer :: seed, get

        print *, "Test beta_distribution_rvs_cdp"
        seed = 639741825
        call random_seed(seed, get)

        ba = (2.0_dp, 0.7_dp); bb = (5.0_dp, 3.0_dp); loc = (0._dp, 0._dp)

        do i = 1, k
            res(i) = rbeta(ba, bb, loc)
        end do
        res(k + 1 : n) = rbeta(ba, bb, k, loc)

        seed = 639741825
        call random_seed(seed, get)
        do i = 1, n
            ans(i) = rbeta(ba, bb, loc)
        end do

        call check(all(abs(res - ans) < dptol),                           &
                   msg="beta_distribution_rvs_cdp failed",      &
                   warn=warn)
    end subroutine test_beta_rvs_cdp


    subroutine test_beta_pdf_rsp
        real(sp) :: x1, x2(3,4), ba, bb, loc
                integer, parameter :: n = 15
        integer :: i
                real(sp) :: res(n)
                real(sp), parameter :: ans(*) =                                    &
                 [2.45759999999999978E+00_sp,            &
                  2.45759999999999978E+00_sp,            &
                  2.45759999999999978E+00_sp,            &
                  1.22175937500000087E+00_sp,            &
                  1.96829999999999972E+00_sp,            &
                  2.34902812499999891E+00_sp,            &
                  2.37304687500000044E+00_sp,            &
                  2.16089999999999938E+00_sp,            &
                  1.87431562500000037E+00_sp,            &
                  1.55520000000000058E+00_sp,            &
                  1.23533437500000032E+00_sp,            &
                  9.37499999999999889E-01_sp,            &
                  4.60800000000000043E-01_sp,            &
                  1.70099999999999946E-01_sp,            &
                  3.83999999999999897E-02_sp]

        print *, "Test beta_distribution_pdf_rsp"
        ba = 2.0_sp
        bb = 5.0_sp
        loc = 0._sp
        x1 = 0.2_sp
        x2 = reshape([0.05_sp, 0.1_sp, 0.15_sp, 0.25_sp,      &
                      0.3_sp, 0.35_sp, 0.4_sp, 0.45_sp,       &
                      0.5_sp, 0.6_sp, 0.7_sp, 0.8_sp], [3,4])

        res(1:3) = beta_pdf(x1, ba, bb, loc)
        res(4:15) = reshape(beta_pdf(x2, ba, bb, loc), [12])

        call check(all(abs(res - ans) < max(sptol, 1.0e-11_sp)),      &
                   msg="beta_distribution_pdf_rsp failed",      &
                   warn=warn)
    end subroutine test_beta_pdf_rsp

    subroutine test_beta_pdf_rdp
        real(dp) :: x1, x2(3,4), ba, bb, loc
                integer, parameter :: n = 15
        integer :: i
                real(dp) :: res(n)
                real(dp), parameter :: ans(*) =                                    &
                 [2.45759999999999978E+00_dp,            &
                  2.45759999999999978E+00_dp,            &
                  2.45759999999999978E+00_dp,            &
                  1.22175937500000087E+00_dp,            &
                  1.96829999999999972E+00_dp,            &
                  2.34902812499999891E+00_dp,            &
                  2.37304687500000044E+00_dp,            &
                  2.16089999999999938E+00_dp,            &
                  1.87431562500000037E+00_dp,            &
                  1.55520000000000058E+00_dp,            &
                  1.23533437500000032E+00_dp,            &
                  9.37499999999999889E-01_dp,            &
                  4.60800000000000043E-01_dp,            &
                  1.70099999999999946E-01_dp,            &
                  3.83999999999999897E-02_dp]

        print *, "Test beta_distribution_pdf_rdp"
        ba = 2.0_dp
        bb = 5.0_dp
        loc = 0._dp
        x1 = 0.2_dp
        x2 = reshape([0.05_dp, 0.1_dp, 0.15_dp, 0.25_dp,      &
                      0.3_dp, 0.35_dp, 0.4_dp, 0.45_dp,       &
                      0.5_dp, 0.6_dp, 0.7_dp, 0.8_dp], [3,4])

        res(1:3) = beta_pdf(x1, ba, bb, loc)
        res(4:15) = reshape(beta_pdf(x2, ba, bb, loc), [12])

        call check(all(abs(res - ans) < max(dptol, 1.0e-11_dp)),      &
                   msg="beta_distribution_pdf_rdp failed",      &
                   warn=warn)
    end subroutine test_beta_pdf_rdp

    subroutine test_beta_pdf_csp
        complex(sp) :: x1, x2(3,4), ba, bb, loc
                integer, parameter :: n = 15
        integer :: i
                real(sp) :: res(n)
                real(sp), parameter :: ans(*) =                                    &
                 [2.77620563793055197E+00_sp,            &
                  2.77620563793055197E+00_sp,            &
                  2.77620563793055197E+00_sp,            &
                  4.35133599581893460E+00_sp,            &
                  5.11042526155962751E+00_sp,            &
                  3.91417214341107966E+00_sp,            &
                  3.25033085838892477E+00_sp,            &
                  2.44104116333175813E+00_sp,            &
                  1.74312991236416215E+00_sp,            &
                  1.18399939600181159E+00_sp,            &
                  7.62828448855675023E-01_sp,            &
                  4.63554726571548781E-01_sp,            &
                  1.79353320295729979E-01_sp,            &
                  5.09635475047793968E-02_sp,            &
                  6.17909755246391895E-03_sp]

        print *, "Test beta_distribution_pdf_csp"
        ba = (2.0_sp, 0.7_sp)
        bb = (5.0_sp, 3.0_sp)
        loc = (0._sp, 0._sp)
        x1 = (0.2_sp, 0.3_sp)
        x2 = reshape([(0.05_sp, 0.05_sp), (0.1_sp, 0.1_sp),   &
                      (0.15_sp, 0.2_sp), (0.25_sp, 0.25_sp),   &
                      (0.3_sp, 0.3_sp), (0.35_sp, 0.35_sp),    &
                      (0.4_sp, 0.4_sp), (0.45_sp, 0.45_sp),    &
                      (0.5_sp, 0.5_sp), (0.6_sp, 0.55_sp),     &
                      (0.7_sp, 0.6_sp), (0.8_sp, 0.7_sp)], [3,4])

        res(1:3) = beta_pdf(x1, ba, bb, loc)
        res(4:15) = reshape(beta_pdf(x2, ba, bb, loc), [12])

        call check(all(abs(res - ans) < max(sptol, 1.0e-11_sp)),      &
                   msg="beta_distribution_pdf_csp failed",      &
                   warn=warn)
    end subroutine test_beta_pdf_csp

    subroutine test_beta_pdf_cdp
        complex(dp) :: x1, x2(3,4), ba, bb, loc
                integer, parameter :: n = 15
        integer :: i
                real(dp) :: res(n)
                real(dp), parameter :: ans(*) =                                    &
                 [2.77620563793055197E+00_dp,            &
                  2.77620563793055197E+00_dp,            &
                  2.77620563793055197E+00_dp,            &
                  4.35133599581893460E+00_dp,            &
                  5.11042526155962751E+00_dp,            &
                  3.91417214341107966E+00_dp,            &
                  3.25033085838892477E+00_dp,            &
                  2.44104116333175813E+00_dp,            &
                  1.74312991236416215E+00_dp,            &
                  1.18399939600181159E+00_dp,            &
                  7.62828448855675023E-01_dp,            &
                  4.63554726571548781E-01_dp,            &
                  1.79353320295729979E-01_dp,            &
                  5.09635475047793968E-02_dp,            &
                  6.17909755246391895E-03_dp]

        print *, "Test beta_distribution_pdf_cdp"
        ba = (2.0_dp, 0.7_dp)
        bb = (5.0_dp, 3.0_dp)
        loc = (0._dp, 0._dp)
        x1 = (0.2_dp, 0.3_dp)
        x2 = reshape([(0.05_dp, 0.05_dp), (0.1_dp, 0.1_dp),   &
                      (0.15_dp, 0.2_dp), (0.25_dp, 0.25_dp),   &
                      (0.3_dp, 0.3_dp), (0.35_dp, 0.35_dp),    &
                      (0.4_dp, 0.4_dp), (0.45_dp, 0.45_dp),    &
                      (0.5_dp, 0.5_dp), (0.6_dp, 0.55_dp),     &
                      (0.7_dp, 0.6_dp), (0.8_dp, 0.7_dp)], [3,4])

        res(1:3) = beta_pdf(x1, ba, bb, loc)
        res(4:15) = reshape(beta_pdf(x2, ba, bb, loc), [12])

        call check(all(abs(res - ans) < max(dptol, 1.0e-11_dp)),      &
                   msg="beta_distribution_pdf_cdp failed",      &
                   warn=warn)
    end subroutine test_beta_pdf_cdp


    subroutine test_beta_cdf_rsp
        real(sp) :: x1, x2(3,4), ba, bb, loc
        integer :: i
        real(sp) :: res(15)
        real(sp), parameter :: ans(15) =                                   &
                 [3.44640000000000224E-01_sp,            &
                  3.44640000000000224E-01_sp,            &
                  3.44640000000000224E-01_sp,            &
                  3.27738281249999944E-02_sp,            &
                  1.14265000000000019E-01_sp,            &
                  2.23515703125000020E-01_sp,            &
                  4.66064453125000000E-01_sp,            &
                  5.79825000000000257E-01_sp,            &
                  6.80920078124999995E-01_sp,            &
                  7.66720000000000068E-01_sp,            &
                  8.36432578124999937E-01_sp,            &
                  8.90625000000000000E-01_sp,            &
                  9.59040000000000004E-01_sp,            &
                  9.89064999999999972E-01_sp,            &
                  9.98399999999999954E-01_sp]

        print *, "Test beta_distribution_cdf_rsp"
        ba = 2.0_sp; bb = 5.0_sp; loc = 0._sp
        x1 = 0.2_sp
        x2 = reshape([0.05_sp, 0.1_sp, 0.15_sp, 0.25_sp,      &
                      0.3_sp, 0.35_sp, 0.4_sp, 0.45_sp,       &
                      0.5_sp, 0.6_sp, 0.7_sp, 0.8_sp], [3,4])

        res(1:3) = beta_cdf(x1, ba, bb, loc)
        res(4:15) = reshape(beta_cdf(x2, ba, bb, loc), [12])

        call check(all(abs(res - ans) < max(sptol, 1.0e-11_sp)),      &
                   msg="beta_distribution_cdf_rsp failed",      &
                   warn=warn)
    end subroutine test_beta_cdf_rsp

    subroutine test_beta_cdf_rdp
        real(dp) :: x1, x2(3,4), ba, bb, loc
        integer :: i
        real(dp) :: res(15)
        real(dp), parameter :: ans(15) =                                   &
                 [3.44640000000000224E-01_dp,            &
                  3.44640000000000224E-01_dp,            &
                  3.44640000000000224E-01_dp,            &
                  3.27738281249999944E-02_dp,            &
                  1.14265000000000019E-01_dp,            &
                  2.23515703125000020E-01_dp,            &
                  4.66064453125000000E-01_dp,            &
                  5.79825000000000257E-01_dp,            &
                  6.80920078124999995E-01_dp,            &
                  7.66720000000000068E-01_dp,            &
                  8.36432578124999937E-01_dp,            &
                  8.90625000000000000E-01_dp,            &
                  9.59040000000000004E-01_dp,            &
                  9.89064999999999972E-01_dp,            &
                  9.98399999999999954E-01_dp]

        print *, "Test beta_distribution_cdf_rdp"
        ba = 2.0_dp; bb = 5.0_dp; loc = 0._dp
        x1 = 0.2_dp
        x2 = reshape([0.05_dp, 0.1_dp, 0.15_dp, 0.25_dp,      &
                      0.3_dp, 0.35_dp, 0.4_dp, 0.45_dp,       &
                      0.5_dp, 0.6_dp, 0.7_dp, 0.8_dp], [3,4])

        res(1:3) = beta_cdf(x1, ba, bb, loc)
        res(4:15) = reshape(beta_cdf(x2, ba, bb, loc), [12])

        call check(all(abs(res - ans) < max(dptol, 1.0e-11_dp)),      &
                   msg="beta_distribution_cdf_rdp failed",      &
                   warn=warn)
    end subroutine test_beta_cdf_rdp

    subroutine test_beta_cdf_csp
        complex(sp) :: x1, x2(3,4), ba, bb, loc
        integer :: i
        real(sp) :: res(15)
        real(sp), parameter :: ans(15) =                                   &
                 [2.64331290012673414E-01_sp,            &
                  2.64331290012673414E-01_sp,            &
                  2.64331290012673414E-01_sp,            &
                  8.86382195944738702E-03_sp,            &
                  4.81500626030448228E-02_sp,            &
                  1.40607931860570912E-01_sp,            &
                  3.28430860699885308E-01_sp,            &
                  4.44713005546652551E-01_sp,            &
                  5.57213246702040532E-01_sp,            &
                  6.59756977764087704E-01_sp,            &
                  7.48497876310954546E-01_sp,            &
                  8.21680689855777691E-01_sp,            &
                  9.05920357339824456E-01_sp,            &
                  9.51254168091690833E-01_sp,            &
                  9.82801096697218157E-01_sp]

        print *, "Test beta_distribution_cdf_csp"
        ba = (2.0_sp, 0.7_sp); bb = (5.0_sp, 3.0_sp); loc = (0._sp, 0._sp)
        x1 = (0.2_sp, 0.3_sp)
        x2 = reshape([(0.05_sp, 0.05_sp), (0.1_sp, 0.1_sp),   &
                      (0.15_sp, 0.2_sp), (0.25_sp, 0.25_sp),   &
                      (0.3_sp, 0.3_sp), (0.35_sp, 0.35_sp),    &
                      (0.4_sp, 0.4_sp), (0.45_sp, 0.45_sp),    &
                      (0.5_sp, 0.5_sp), (0.6_sp, 0.55_sp),     &
                      (0.7_sp, 0.6_sp), (0.8_sp, 0.7_sp)], [3,4])

        res(1:3) = beta_cdf(x1, ba, bb, loc)
        res(4:15) = reshape(beta_cdf(x2, ba, bb, loc), [12])

        call check(all(abs(res - ans) < max(sptol, 1.0e-11_sp)),      &
                   msg="beta_distribution_cdf_csp failed",      &
                   warn=warn)
    end subroutine test_beta_cdf_csp

    subroutine test_beta_cdf_cdp
        complex(dp) :: x1, x2(3,4), ba, bb, loc
        integer :: i
        real(dp) :: res(15)
        real(dp), parameter :: ans(15) =                                   &
                 [2.64331290012673414E-01_dp,            &
                  2.64331290012673414E-01_dp,            &
                  2.64331290012673414E-01_dp,            &
                  8.86382195944738702E-03_dp,            &
                  4.81500626030448228E-02_dp,            &
                  1.40607931860570912E-01_dp,            &
                  3.28430860699885308E-01_dp,            &
                  4.44713005546652551E-01_dp,            &
                  5.57213246702040532E-01_dp,            &
                  6.59756977764087704E-01_dp,            &
                  7.48497876310954546E-01_dp,            &
                  8.21680689855777691E-01_dp,            &
                  9.05920357339824456E-01_dp,            &
                  9.51254168091690833E-01_dp,            &
                  9.82801096697218157E-01_dp]

        print *, "Test beta_distribution_cdf_cdp"
        ba = (2.0_dp, 0.7_dp); bb = (5.0_dp, 3.0_dp); loc = (0._dp, 0._dp)
        x1 = (0.2_dp, 0.3_dp)
        x2 = reshape([(0.05_dp, 0.05_dp), (0.1_dp, 0.1_dp),   &
                      (0.15_dp, 0.2_dp), (0.25_dp, 0.25_dp),   &
                      (0.3_dp, 0.3_dp), (0.35_dp, 0.35_dp),    &
                      (0.4_dp, 0.4_dp), (0.45_dp, 0.45_dp),    &
                      (0.5_dp, 0.5_dp), (0.6_dp, 0.55_dp),     &
                      (0.7_dp, 0.6_dp), (0.8_dp, 0.7_dp)], [3,4])

        res(1:3) = beta_cdf(x1, ba, bb, loc)
        res(4:15) = reshape(beta_cdf(x2, ba, bb, loc), [12])

        call check(all(abs(res - ans) < max(dptol, 1.0e-11_dp)),      &
                   msg="beta_distribution_cdf_cdp failed",      &
                   warn=warn)
    end subroutine test_beta_cdf_cdp


end program test_distribution_beta
