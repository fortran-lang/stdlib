program test_distribution_gamma
    use stdlib_kinds
    use stdlib_error, only : check
    use stdlib_stats_distribution_PRNG, only: random_seed
    use stdlib_stats_distribution_gamma, gamma_rvs => gamma_distribution_rvs,   &
                                         gamma_pdf => gamma_distribution_pdf,   &
                                         gamma_cdf => gamma_distribution_cdf

    implicit none
    real(sp), parameter :: sptol = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1.0_dp)
    real(qp), parameter :: qptol = 1000 * epsilon(1.0_qp)
    logical ::  warn = .true.
    integer :: put, get
	
    put = 1234567
    call random_seed(put, get)
	
    call test_gamma_random_generator

    call test_gamma_rvs_rsp
    call test_gamma_rvs_rdp
    call test_gamma_rvs_rqp
    call test_gamma_rvs_csp
    call test_gamma_rvs_cdp
    call test_gamma_rvs_cqp

    call test_gamma_pdf_rsp
    call test_gamma_pdf_rdp
    call test_gamma_pdf_rqp
    call test_gamma_pdf_csp
    call test_gamma_pdf_cdp
    call test_gamma_pdf_cqp

    call test_gamma_cdf_rsp
    call test_gamma_cdf_rdp
    call test_gamma_cdf_rqp
    call test_gamma_cdf_csp
    call test_gamma_cdf_cdp
    call test_gamma_cdf_cqp


    contains

    subroutine test_gamma_random_generator
        integer :: i, j, freq(0:1000), num=10000000
        real(dp) :: chisq, expct

        print *, ""
        print *, "Test gamma random generator with chi-squared"
        freq = 0
        do i = 1, num
            j = 1000 * gamma_cdf(gamma_rvs(2.0,1.5),2.0,1.5)
            freq(j) = freq(j) + 1
        end do
        chisq = 0.0_dp
        expct = num / 1000
        do i = 0, 999
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do
        write(*,*) "The critical values for chi-squared with 1000 d. of f. is"  &
                   //" 1143.92"
        write(*,*) "Chi-squared for gamma random generator is : ", chisq
        call check((chisq < 1143.9),                                            &
               msg="gamma randomness failed chi-squared test", warn=warn)
    end subroutine test_gamma_random_generator

    subroutine test_gamma_rvs_rsp
        real(sp) :: res(10), gshape, scale
        integer :: i, n, k = 5
        integer :: put, get
        real(sp) :: ans(10) = [0.857589039350877514111471181067133115_sp,     &
                             1.02066235929592669341367273855793615_sp,      &
                             0.997539313039285858469791992057480517_sp,     &
                             0.976533566171099213454202419140525167_sp,     &
                             0.418534850809151373739671312677149231_sp,     &
                             2.20122874546440374485431246113130646_sp,      &
                             2.06395422779089208145254668611859318_sp,      &
                             3.17946689363011574223408637477787452_sp,      &
                             1.93297441375957258760155732080675223_sp,      &
                             1.02579597344383310585282655020137840_sp]

        print *, "Test gamma_distribution_rvs_rsp"
        put = 639741825
        call random_seed(put, get)
        gshape = 2.0_sp; scale = 1.0_sp
        do i = 1, 5
            res(i) = gamma_rvs(gshape, scale)
        end do
        res(6:10) = gamma_rvs(gshape, scale, k)
        call check(all(abs(res - ans) < sptol), &
            msg="gamma_distribution_rvs_rsp failed", warn=warn)
    end subroutine test_gamma_rvs_rsp

    subroutine test_gamma_rvs_rdp
        real(dp) :: res(10), gshape, scale
        integer :: i, n, k = 5
        integer :: put, get
        real(dp) :: ans(10) = [0.857589039350877514111471181067133115_dp,     &
                             1.02066235929592669341367273855793615_dp,      &
                             0.997539313039285858469791992057480517_dp,     &
                             0.976533566171099213454202419140525167_dp,     &
                             0.418534850809151373739671312677149231_dp,     &
                             2.20122874546440374485431246113130646_dp,      &
                             2.06395422779089208145254668611859318_dp,      &
                             3.17946689363011574223408637477787452_dp,      &
                             1.93297441375957258760155732080675223_dp,      &
                             1.02579597344383310585282655020137840_dp]

        print *, "Test gamma_distribution_rvs_rdp"
        put = 639741825
        call random_seed(put, get)
        gshape = 2.0_dp; scale = 1.0_dp
        do i = 1, 5
            res(i) = gamma_rvs(gshape, scale)
        end do
        res(6:10) = gamma_rvs(gshape, scale, k)
        call check(all(abs(res - ans) < dptol), &
            msg="gamma_distribution_rvs_rdp failed", warn=warn)
    end subroutine test_gamma_rvs_rdp

    subroutine test_gamma_rvs_rqp
        real(qp) :: res(10), gshape, scale
        integer :: i, n, k = 5
        integer :: put, get
        real(qp) :: ans(10) = [0.857589039350877514111471181067133115_qp,     &
                             1.02066235929592669341367273855793615_qp,      &
                             0.997539313039285858469791992057480517_qp,     &
                             0.976533566171099213454202419140525167_qp,     &
                             0.418534850809151373739671312677149231_qp,     &
                             2.20122874546440374485431246113130646_qp,      &
                             2.06395422779089208145254668611859318_qp,      &
                             3.17946689363011574223408637477787452_qp,      &
                             1.93297441375957258760155732080675223_qp,      &
                             1.02579597344383310585282655020137840_qp]

        print *, "Test gamma_distribution_rvs_rqp"
        put = 639741825
        call random_seed(put, get)
        gshape = 2.0_qp; scale = 1.0_qp
        do i = 1, 5
            res(i) = gamma_rvs(gshape, scale)
        end do
        res(6:10) = gamma_rvs(gshape, scale, k)
        call check(all(abs(res - ans) < qptol), &
            msg="gamma_distribution_rvs_rqp failed", warn=warn)
    end subroutine test_gamma_rvs_rqp

    subroutine test_gamma_rvs_csp
        complex(sp) :: res(10), gshape, scale
        integer :: i, n, k = 5
        integer :: put, get
        complex(sp) :: ans(10) = [(1.07198631763458251953125000000000000_sp,     &
                            0.467755347490310668945312500000000000_sp),     &
                            (0.423825174570083618164062500000000000_sp,     &
                            0.963404953479766845703125000000000000_sp),     &
                             (2.75153589248657226562500000000000000_sp,     &
                            0.148371994495391845703125000000000000_sp),     &
                             (1.45363664627075195312500000000000000_sp,     &
                            0.568527400493621826171875000000000000_sp),     &
                            (0.345591425895690917968750000000000000_sp,     &
                      4.962176829576492309570312500000000000E-0002_sp),     &
                             (1.96578848361968994140625000000000000_sp,     &
                             3.11243152618408203125000000000000000_sp),     &
                             (3.41551613807678222656250000000000000_sp,     &
                      5.049489438533782958984375000000000000E-0002_sp),     &
                            (0.945943951606750488281250000000000000_sp,     &
                            0.456915855407714843750000000000000000_sp),     &
                             (1.14931583404541015625000000000000000_sp,     &
                            0.129447638988494873046875000000000000_sp),     &
                             (2.96914696693420410156250000000000000_sp,     &
                             1.16174089908599853515625000000000000_sp)]

        print *, "Test gamma_distribution_rvs_csp"
        put = 639741825
        call random_seed(put, get)
        gshape = (2.0_sp, 0.7_sp); scale = (0.8_sp, 1.2_sp)
        do i = 1, 5
            res(i) = gamma_rvs(gshape, scale)
        end do
        res(6:10) = gamma_rvs(gshape, scale, k)
        call check(all(abs(res - ans) < sptol), &
            msg="gamma_distribution_rvs_csp failed", warn=warn)
    end subroutine test_gamma_rvs_csp

    subroutine test_gamma_rvs_cdp
        complex(dp) :: res(10), gshape, scale
        integer :: i, n, k = 5
        integer :: put, get
        complex(dp) :: ans(10) = [(1.07198631763458251953125000000000000_dp,     &
                            0.467755347490310668945312500000000000_dp),     &
                            (0.423825174570083618164062500000000000_dp,     &
                            0.963404953479766845703125000000000000_dp),     &
                             (2.75153589248657226562500000000000000_dp,     &
                            0.148371994495391845703125000000000000_dp),     &
                             (1.45363664627075195312500000000000000_dp,     &
                            0.568527400493621826171875000000000000_dp),     &
                            (0.345591425895690917968750000000000000_dp,     &
                      4.962176829576492309570312500000000000E-0002_dp),     &
                             (1.96578848361968994140625000000000000_dp,     &
                             3.11243152618408203125000000000000000_dp),     &
                             (3.41551613807678222656250000000000000_dp,     &
                      5.049489438533782958984375000000000000E-0002_dp),     &
                            (0.945943951606750488281250000000000000_dp,     &
                            0.456915855407714843750000000000000000_dp),     &
                             (1.14931583404541015625000000000000000_dp,     &
                            0.129447638988494873046875000000000000_dp),     &
                             (2.96914696693420410156250000000000000_dp,     &
                             1.16174089908599853515625000000000000_dp)]

        print *, "Test gamma_distribution_rvs_cdp"
        put = 639741825
        call random_seed(put, get)
        gshape = (2.0_dp, 0.7_dp); scale = (0.8_dp, 1.2_dp)
        do i = 1, 5
            res(i) = gamma_rvs(gshape, scale)
        end do
        res(6:10) = gamma_rvs(gshape, scale, k)
        call check(all(abs(res - ans) < dptol), &
            msg="gamma_distribution_rvs_cdp failed", warn=warn)
    end subroutine test_gamma_rvs_cdp

    subroutine test_gamma_rvs_cqp
        complex(qp) :: res(10), gshape, scale
        integer :: i, n, k = 5
        integer :: put, get
        complex(qp) :: ans(10) = [(1.07198631763458251953125000000000000_qp,     &
                            0.467755347490310668945312500000000000_qp),     &
                            (0.423825174570083618164062500000000000_qp,     &
                            0.963404953479766845703125000000000000_qp),     &
                             (2.75153589248657226562500000000000000_qp,     &
                            0.148371994495391845703125000000000000_qp),     &
                             (1.45363664627075195312500000000000000_qp,     &
                            0.568527400493621826171875000000000000_qp),     &
                            (0.345591425895690917968750000000000000_qp,     &
                      4.962176829576492309570312500000000000E-0002_qp),     &
                             (1.96578848361968994140625000000000000_qp,     &
                             3.11243152618408203125000000000000000_qp),     &
                             (3.41551613807678222656250000000000000_qp,     &
                      5.049489438533782958984375000000000000E-0002_qp),     &
                            (0.945943951606750488281250000000000000_qp,     &
                            0.456915855407714843750000000000000000_qp),     &
                             (1.14931583404541015625000000000000000_qp,     &
                            0.129447638988494873046875000000000000_qp),     &
                             (2.96914696693420410156250000000000000_qp,     &
                             1.16174089908599853515625000000000000_qp)]

        print *, "Test gamma_distribution_rvs_cqp"
        put = 639741825
        call random_seed(put, get)
        gshape = (2.0_qp, 0.7_qp); scale = (0.8_qp, 1.2_qp)
        do i = 1, 5
            res(i) = gamma_rvs(gshape, scale)
        end do
        res(6:10) = gamma_rvs(gshape, scale, k)
        call check(all(abs(res - ans) < qptol), &
            msg="gamma_distribution_rvs_cqp failed", warn=warn)
    end subroutine test_gamma_rvs_cqp



    subroutine test_gamma_pdf_rsp
        real(sp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [3.44954208E-02, 3.44954208E-02, 3.44954208E-02,      &
                           0.291166335, 0.283382922, 0.279222697, 0.364406645,  &
                           0.243792102,6.38156384E-02,0.258446008, 0.172681183, &
                           0.311812222, 0.240270957, 0.367655009, 9.90117192E-02]

        print *, "Test gamma_distribution_pdf_rsp"
        put = 345987126
        call random_seed(put, get)
        gshape = 2.0_sp; scale = 1.0_sp
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_pdf(x1, gshape, scale)
        res(:, 2:5) = gamma_pdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="gamma_distribution_pdf_rsp failed", warn=warn)
    end subroutine test_gamma_pdf_rsp

    subroutine test_gamma_pdf_rdp
        real(dp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [3.44954208E-02, 3.44954208E-02, 3.44954208E-02,      &
                           0.291166335, 0.283382922, 0.279222697, 0.364406645,  &
                           0.243792102,6.38156384E-02,0.258446008, 0.172681183, &
                           0.311812222, 0.240270957, 0.367655009, 9.90117192E-02]

        print *, "Test gamma_distribution_pdf_rdp"
        put = 345987126
        call random_seed(put, get)
        gshape = 2.0_dp; scale = 1.0_dp
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_pdf(x1, gshape, scale)
        res(:, 2:5) = gamma_pdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol), &
            msg="gamma_distribution_pdf_rdp failed", warn=warn)
    end subroutine test_gamma_pdf_rdp

    subroutine test_gamma_pdf_rqp
        real(qp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [3.44954208E-02, 3.44954208E-02, 3.44954208E-02,      &
                           0.291166335, 0.283382922, 0.279222697, 0.364406645,  &
                           0.243792102,6.38156384E-02,0.258446008, 0.172681183, &
                           0.311812222, 0.240270957, 0.367655009, 9.90117192E-02]

        print *, "Test gamma_distribution_pdf_rqp"
        put = 345987126
        call random_seed(put, get)
        gshape = 2.0_qp; scale = 1.0_qp
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_pdf(x1, gshape, scale)
        res(:, 2:5) = gamma_pdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < qptol), &
            msg="gamma_distribution_pdf_rqp failed", warn=warn)
    end subroutine test_gamma_pdf_rqp

    subroutine test_gamma_pdf_csp
        complex(sp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [0.115542844,0.115542844,0.115542844, 9.26823243E-02, &
                           0.401668519, 0.374689817, 0.147123635, 0.225616276,  &
                           0.127654046, 3.91825065E-02, 2.58735381E-03,         &
                           0.101058327, 0.240440935, 4.98853484E-03, 0.110858262]

        print *, "Test gamma_distribution_pdf_csp"
        put = 345987126
        call random_seed(put, get)
        gshape = (2.0_sp, 0.7_sp); scale = (0.8_sp, 1.2_sp)
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_pdf(x1, gshape, scale)
        res(:, 2:5) = gamma_pdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="gamma_distribution_pdf_csp failed", warn=warn)
    end subroutine test_gamma_pdf_csp

    subroutine test_gamma_pdf_cdp
        complex(dp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [0.115542844,0.115542844,0.115542844, 9.26823243E-02, &
                           0.401668519, 0.374689817, 0.147123635, 0.225616276,  &
                           0.127654046, 3.91825065E-02, 2.58735381E-03,         &
                           0.101058327, 0.240440935, 4.98853484E-03, 0.110858262]

        print *, "Test gamma_distribution_pdf_cdp"
        put = 345987126
        call random_seed(put, get)
        gshape = (2.0_dp, 0.7_dp); scale = (0.8_dp, 1.2_dp)
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_pdf(x1, gshape, scale)
        res(:, 2:5) = gamma_pdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol), &
            msg="gamma_distribution_pdf_cdp failed", warn=warn)
    end subroutine test_gamma_pdf_cdp

    subroutine test_gamma_pdf_cqp
        complex(qp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [0.115542844,0.115542844,0.115542844, 9.26823243E-02, &
                           0.401668519, 0.374689817, 0.147123635, 0.225616276,  &
                           0.127654046, 3.91825065E-02, 2.58735381E-03,         &
                           0.101058327, 0.240440935, 4.98853484E-03, 0.110858262]

        print *, "Test gamma_distribution_pdf_cqp"
        put = 345987126
        call random_seed(put, get)
        gshape = (2.0_qp, 0.7_qp); scale = (0.8_qp, 1.2_qp)
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_pdf(x1, gshape, scale)
        res(:, 2:5) = gamma_pdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < qptol), &
            msg="gamma_distribution_pdf_cqp failed", warn=warn)
    end subroutine test_gamma_pdf_cqp


    subroutine test_gamma_cdf_rsp
        real(sp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [5.48762567E-02, 5.48762567E-02, 5.48762567E-02,      &
                           0.315411955, 0.385681599, 0.232208580, 0.393366873,  &
                           0.805594206, 0.886319339, 0.376679629, 0.141763687,  &
                           0.455908805, 0.278569371, 0.181033060, 0.729863822]

        print *, "Test gamma_distribution_cdf_rsp"
        put = 567985123
        call random_seed(put, get)
        gshape = 2.0_sp; scale = 2.0_sp
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_cdf(x1, gshape, scale)
        res(:, 2:5) = gamma_cdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="gamma_distribution_cdf_rsp failed", warn=warn)
    end subroutine test_gamma_cdf_rsp

    subroutine test_gamma_cdf_rdp
        real(dp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [5.48762567E-02, 5.48762567E-02, 5.48762567E-02,      &
                           0.315411955, 0.385681599, 0.232208580, 0.393366873,  &
                           0.805594206, 0.886319339, 0.376679629, 0.141763687,  &
                           0.455908805, 0.278569371, 0.181033060, 0.729863822]

        print *, "Test gamma_distribution_cdf_rdp"
        put = 567985123
        call random_seed(put, get)
        gshape = 2.0_dp; scale = 2.0_dp
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_cdf(x1, gshape, scale)
        res(:, 2:5) = gamma_cdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol), &
            msg="gamma_distribution_cdf_rdp failed", warn=warn)
    end subroutine test_gamma_cdf_rdp

    subroutine test_gamma_cdf_rqp
        real(qp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [5.48762567E-02, 5.48762567E-02, 5.48762567E-02,      &
                           0.315411955, 0.385681599, 0.232208580, 0.393366873,  &
                           0.805594206, 0.886319339, 0.376679629, 0.141763687,  &
                           0.455908805, 0.278569371, 0.181033060, 0.729863822]

        print *, "Test gamma_distribution_cdf_rqp"
        put = 567985123
        call random_seed(put, get)
        gshape = 2.0_qp; scale = 2.0_qp
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_cdf(x1, gshape, scale)
        res(:, 2:5) = gamma_cdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < qptol), &
            msg="gamma_distribution_cdf_rqp failed", warn=warn)
    end subroutine test_gamma_cdf_rqp

    subroutine test_gamma_cdf_csp
        complex(sp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [3.21221203E-02, 3.21221203E-02, 3.21221203E-02,      &
                           0.209311500,0.779570222, 0.170826405, 2.75949780E-02,&
                           2.37940717E-02, 5.22981845E-02, 0.223270506,         &
                           0.273653150, 3.49688679E-02,0.580260038, 0.230904028,&
                           0.250726104]

        print *, "Test gamma_distribution_cdf_csp"
        put = 567985123
        call random_seed(put, get)
        gshape = (2.0_sp, 0.7_sp); scale = (0.8_sp, 1.2_sp)
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_cdf(x1, gshape, scale)
        res(:, 2:5) = gamma_cdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="gamma_distribution_cdf_csp failed", warn=warn)
    end subroutine test_gamma_cdf_csp

    subroutine test_gamma_cdf_cdp
        complex(dp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [3.21221203E-02, 3.21221203E-02, 3.21221203E-02,      &
                           0.209311500,0.779570222, 0.170826405, 2.75949780E-02,&
                           2.37940717E-02, 5.22981845E-02, 0.223270506,         &
                           0.273653150, 3.49688679E-02,0.580260038, 0.230904028,&
                           0.250726104]

        print *, "Test gamma_distribution_cdf_cdp"
        put = 567985123
        call random_seed(put, get)
        gshape = (2.0_dp, 0.7_dp); scale = (0.8_dp, 1.2_dp)
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_cdf(x1, gshape, scale)
        res(:, 2:5) = gamma_cdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol), &
            msg="gamma_distribution_cdf_cdp failed", warn=warn)
    end subroutine test_gamma_cdf_cdp

    subroutine test_gamma_cdf_cqp
        complex(qp) :: x1, x2(3,4), gshape, scale
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [3.21221203E-02, 3.21221203E-02, 3.21221203E-02,      &
                           0.209311500,0.779570222, 0.170826405, 2.75949780E-02,&
                           2.37940717E-02, 5.22981845E-02, 0.223270506,         &
                           0.273653150, 3.49688679E-02,0.580260038, 0.230904028,&
                           0.250726104]

        print *, "Test gamma_distribution_cdf_cqp"
        put = 567985123
        call random_seed(put, get)
        gshape = (2.0_qp, 0.7_qp); scale = (0.8_qp, 1.2_qp)
        x1 = gamma_rvs(gshape, scale)
        x2 = reshape(gamma_rvs(gshape, scale, 12), [3,4])
        res(:,1) = gamma_cdf(x1, gshape, scale)
        res(:, 2:5) = gamma_cdf(x2, gshape, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < qptol), &
            msg="gamma_distribution_cdf_cqp failed", warn=warn)
    end subroutine test_gamma_cdf_cqp

end program test_distribution_gamma