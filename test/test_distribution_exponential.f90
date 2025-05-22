
program test_distribution_expon
    use stdlib_kinds, only : sp, dp, xdp, qp
    use stdlib_error, only : check
    use stdlib_random, only : random_seed
    use stdlib_stats_distribution_exponential, only : expon_rvs => rvs_exp,  &
                              expon_pdf => pdf_exp, expon_cdf => cdf_exp

    implicit none
    real(sp), parameter :: sptol = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1.0_dp)
    logical ::  warn = .true.
    integer :: put, get

    put = 12345678
    call random_seed(put, get)


    call test_exponential_random_generator


    call test_expon_rvs_rsp
    call test_expon_rvs_rdp
    call test_expon_rvs_csp
    call test_expon_rvs_cdp


    call test_expon_pdf_rsp
    call test_expon_pdf_rdp
    call test_expon_pdf_csp
    call test_expon_pdf_cdp
    call test_expon_pdf_rsp


    call test_expon_cdf_rsp
    call test_expon_cdf_rdp
    call test_expon_cdf_csp
    call test_expon_cdf_cdp





contains

    subroutine test_exponential_random_generator

        integer, parameter :: num = 10000000, array_size = 1000
        integer :: i, j, freq(0:array_size)
        real(dp) :: chisq, expct

        print *, ""
        print *, "Test exponential random generator with chi-squared"

        ! using interface for lambda
        freq = 0
        do i = 1, num
            j = 1000 * (1 - exp(- expon_rvs(1.0)))
            freq(j) = freq(j) + 1
        end do
        chisq = 0.0_dp
        expct = num / array_size
        do i = 0, array_size - 1
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do
        write(*,*) "The critical values for chi-squared with 1000 d. of f. is" &
            //" 1143.92"
        write(*,*) "Chi-squared for exponential random generator is : ", chisq
        call check((chisq < 1143.9), &
            msg="exponential randomness failed chi-squared test", warn=warn)

        ! using interface for loc and scale
        freq = 0
        do i = 1, num
            j = 1000 * (1 - exp(- expon_rvs(0.0, 1.0)))
            freq(j) = freq(j) + 1
        end do
        chisq = 0.0_dp
        expct = num / array_size
        do i = 0, array_size - 1
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do
        write(*,*) "The critical values for chi-squared with 1000 d. of f. is" &
            //" 1143.92"
        write(*,*) "Chi-squared for exponential random generator is : ", chisq
        call check((chisq < 1143.9), &
            msg="exponential randomness failed chi-squared test", warn=warn)

    end subroutine test_exponential_random_generator



    subroutine test_expon_rvs_rsp
        real(sp) :: res(10), lambda, loc, scale
        integer, parameter :: k = 5
        integer :: i
        integer :: seed, get
        real(sp), parameter :: ans(10) =                                         &
                            [0.609680481289574416337018192280083895_sp,    &
                             0.137541023585612635452927558314210417_sp,    &
                             0.134921508232253721063879462841820585_sp,    &
                              1.33766060689229752493171569464417802_sp,    &
                             0.111148487576340881943792737729381770_sp,    &
                             0.533951653963536868966836361020492979_sp,    &
                              1.96897428558727671799033487332053483_sp,    &
                             0.371111977992924465160247867364281152_sp,    &
                             0.811918715695663687862785688290993341_sp,    &
                             0.404637854946697868759504975362991277_sp]

        print *, "Test exponential_distribution_rvs_rsp"
        seed = 593742186

        ! set args
        lambda = 1.5_sp
        loc    = 0._sp
        scale  = 1.0_sp/lambda

        ! tests using interface for lambda
        call random_seed(seed, get)
        do i = 1, k
            res(i) = expon_rvs(lambda)      ! 1 dummy
        end do
        res(6:10) = expon_rvs(lambda, k)    ! 2 dummies
        call check(all(abs(res - ans) < sptol),                            &
            msg="exponential_distribution_rvs_rsp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        do i = 1, k
            res(i) = expon_rvs(loc, scale)
        end do
        res(6:10) = expon_rvs(loc, scale, k)
        call check(all(abs(res - ans) < sptol),                            &
            msg="exponential_distribution_rvs_rsp failed", warn=warn)
    end subroutine test_expon_rvs_rsp

    subroutine test_expon_rvs_rdp
        real(dp) :: res(10), lambda, loc, scale
        integer, parameter :: k = 5
        integer :: i
        integer :: seed, get
        real(dp), parameter :: ans(10) =                                         &
                            [0.609680481289574416337018192280083895_dp,    &
                             0.137541023585612635452927558314210417_dp,    &
                             0.134921508232253721063879462841820585_dp,    &
                              1.33766060689229752493171569464417802_dp,    &
                             0.111148487576340881943792737729381770_dp,    &
                             0.533951653963536868966836361020492979_dp,    &
                              1.96897428558727671799033487332053483_dp,    &
                             0.371111977992924465160247867364281152_dp,    &
                             0.811918715695663687862785688290993341_dp,    &
                             0.404637854946697868759504975362991277_dp]

        print *, "Test exponential_distribution_rvs_rdp"
        seed = 593742186

        ! set args
        lambda = 1.5_dp
        loc    = 0._dp
        scale  = 1.0_dp/lambda

        ! tests using interface for lambda
        call random_seed(seed, get)
        do i = 1, k
            res(i) = expon_rvs(lambda)      ! 1 dummy
        end do
        res(6:10) = expon_rvs(lambda, k)    ! 2 dummies
        call check(all(abs(res - ans) < dptol),                            &
            msg="exponential_distribution_rvs_rdp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        do i = 1, k
            res(i) = expon_rvs(loc, scale)
        end do
        res(6:10) = expon_rvs(loc, scale, k)
        call check(all(abs(res - ans) < dptol),                            &
            msg="exponential_distribution_rvs_rdp failed", warn=warn)
    end subroutine test_expon_rvs_rdp

    subroutine test_expon_rvs_csp
        complex(sp) :: res(10), lambda, loc, scale
        integer, parameter :: k = 5
        integer :: i
        integer :: seed, get
        complex(sp), parameter :: ans(10) =                                         &
                            [(1.30645817419194517786503898345732266_sp,    &
                             0.158701181060322271676454874977935106_sp),   &
                            (0.289117517640543687994027420375329869_sp,    &
                              1.54345454641418945184428733997405138_sp),   &
                            (0.238175330520730461308127295134389521_sp,    &
                             0.616098062265619464192503493485184250_sp),   &
                             (4.21923061197273582426500329997257485_sp,    &
                             0.428206128453374382877209077728016710_sp),   &
                             (1.73982581934785075970596933205212874_sp,    &
                             0.466889832630805233184044202341912994_sp),   &
                             (2.22649889847873832288931745486999202_sp,    &
                             0.879109337848515628785697537331053851_sp),   &
                             (8.76802198822945553859296653951917464_sp,    &
                             0.200128045239398311139211728004738688_sp),   &
                            (0.694821947760945587572020290930855262_sp,    &
                             0.101964167346166995492113143812345625_sp),   &
                            (0.141476585024528208770330398432893829_sp,    &
                       3.989655879458742013468417133900891716E-0002_sp),   &
                             (2.10676792861163792685325850990401309_sp,    &
                             0.249356813451327473065187125310051027_sp)]

        print *, "Test exponential_distribution_rvs_csp"
        seed = 593742186

        ! set args
        lambda = (0.7_sp, 1.3_sp)
        loc    = (0._sp, 0._sp)
        scale  = cmplx(1.0_sp/lambda%re, 1.0_sp/lambda%im, kind=sp)

        ! tests using interface for lambda
        call random_seed(seed, get)
        do i = 1, k
            res(i) = expon_rvs(lambda)      ! 1 dummy
        end do
        res(6:10) = expon_rvs(lambda, k)    ! 2 dummies
        call check(all(abs(res - ans) < sptol),                            &
            msg="exponential_distribution_rvs_csp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        do i = 1, k
            res(i) = expon_rvs(loc, scale)
        end do
        res(6:10) = expon_rvs(loc, scale, k)
        call check(all(abs(res - ans) < sptol),                            &
            msg="exponential_distribution_rvs_csp failed", warn=warn)
    end subroutine test_expon_rvs_csp

    subroutine test_expon_rvs_cdp
        complex(dp) :: res(10), lambda, loc, scale
        integer, parameter :: k = 5
        integer :: i
        integer :: seed, get
        complex(dp), parameter :: ans(10) =                                         &
                            [(1.30645817419194517786503898345732266_dp,    &
                             0.158701181060322271676454874977935106_dp),   &
                            (0.289117517640543687994027420375329869_dp,    &
                              1.54345454641418945184428733997405138_dp),   &
                            (0.238175330520730461308127295134389521_dp,    &
                             0.616098062265619464192503493485184250_dp),   &
                             (4.21923061197273582426500329997257485_dp,    &
                             0.428206128453374382877209077728016710_dp),   &
                             (1.73982581934785075970596933205212874_dp,    &
                             0.466889832630805233184044202341912994_dp),   &
                             (2.22649889847873832288931745486999202_dp,    &
                             0.879109337848515628785697537331053851_dp),   &
                             (8.76802198822945553859296653951917464_dp,    &
                             0.200128045239398311139211728004738688_dp),   &
                            (0.694821947760945587572020290930855262_dp,    &
                             0.101964167346166995492113143812345625_dp),   &
                            (0.141476585024528208770330398432893829_dp,    &
                       3.989655879458742013468417133900891716E-0002_dp),   &
                             (2.10676792861163792685325850990401309_dp,    &
                             0.249356813451327473065187125310051027_dp)]

        print *, "Test exponential_distribution_rvs_cdp"
        seed = 593742186

        ! set args
        lambda = (0.7_dp, 1.3_dp)
        loc    = (0._dp, 0._dp)
        scale  = cmplx(1.0_dp/lambda%re, 1.0_dp/lambda%im, kind=dp)

        ! tests using interface for lambda
        call random_seed(seed, get)
        do i = 1, k
            res(i) = expon_rvs(lambda)      ! 1 dummy
        end do
        res(6:10) = expon_rvs(lambda, k)    ! 2 dummies
        call check(all(abs(res - ans) < dptol),                            &
            msg="exponential_distribution_rvs_cdp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        do i = 1, k
            res(i) = expon_rvs(loc, scale)
        end do
        res(6:10) = expon_rvs(loc, scale, k)
        call check(all(abs(res - ans) < dptol),                            &
            msg="exponential_distribution_rvs_cdp failed", warn=warn)
    end subroutine test_expon_rvs_cdp





    subroutine test_expon_pdf_rsp

        real(sp) :: x1, x2(3,4), lambda, loc, scale
        integer :: seed, get
        real(sp) :: res(3,5)
        real(sp), parameter :: ans(15) =                                   &
                             [0.362692289054629718342313806171796533_sp,   &
                              0.362692289054629718342313806171796533_sp,   &
                              0.362692289054629718342313806171796533_sp,   &
                               1.44877092399186122284289290051705535_sp,   &
                               1.08871761038277651996081144393335589_sp,   &
                              0.203258408490339213767867275283195750_sp,   &
                              0.730004225568590859263284124264208147_sp,   &
                              0.237394827760488451509080387833146683_sp,   &
                              0.301732182586179598102005265289645959_sp,   &
                               1.35079274124711914255014934401469271_sp,   &
                              0.416578245043239337295928202660090263_sp,   &
                               1.44039177901335374382803898226703593_sp,   &
                              0.196044829271295768265275728683411055_sp,   &
                              0.271373826917613661285112379170965958_sp,   &
                               1.00108987409617105109732206933052664_sp]

        print *, "Test exponential_distribution_pdf_rsp"
        seed = 123987654

        ! set args
        lambda = 1.5_sp
        loc    = 0._sp
        scale  = 1.0_sp/lambda

        ! tests using interface for lambda
        call random_seed(seed, get)
        x1 = expon_rvs(lambda)
        x2 = reshape(expon_rvs(lambda, 12), [3,4])
        res(:,1) = expon_pdf(x1, lambda)
        res(:, 2:5) = expon_pdf(x2, lambda)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol),            &
            msg="exponential_distribution_pdf_rsp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        x1 = expon_rvs(loc, scale)
        x2 = reshape(expon_rvs(loc, scale, 12), [3,4])
        res(:,1) = expon_pdf(x1, loc, scale)
        res(:, 2:5) = expon_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol),            &
            msg="exponential_distribution_pdf_rsp failed", warn=warn)

    end subroutine test_expon_pdf_rsp

    subroutine test_expon_pdf_rdp

        real(dp) :: x1, x2(3,4), lambda, loc, scale
        integer :: seed, get
        real(dp) :: res(3,5)
        real(dp), parameter :: ans(15) =                                   &
                             [0.362692289054629718342313806171796533_dp,   &
                              0.362692289054629718342313806171796533_dp,   &
                              0.362692289054629718342313806171796533_dp,   &
                               1.44877092399186122284289290051705535_dp,   &
                               1.08871761038277651996081144393335589_dp,   &
                              0.203258408490339213767867275283195750_dp,   &
                              0.730004225568590859263284124264208147_dp,   &
                              0.237394827760488451509080387833146683_dp,   &
                              0.301732182586179598102005265289645959_dp,   &
                               1.35079274124711914255014934401469271_dp,   &
                              0.416578245043239337295928202660090263_dp,   &
                               1.44039177901335374382803898226703593_dp,   &
                              0.196044829271295768265275728683411055_dp,   &
                              0.271373826917613661285112379170965958_dp,   &
                               1.00108987409617105109732206933052664_dp]

        print *, "Test exponential_distribution_pdf_rdp"
        seed = 123987654

        ! set args
        lambda = 1.5_dp
        loc    = 0._dp
        scale  = 1.0_dp/lambda

        ! tests using interface for lambda
        call random_seed(seed, get)
        x1 = expon_rvs(lambda)
        x2 = reshape(expon_rvs(lambda, 12), [3,4])
        res(:,1) = expon_pdf(x1, lambda)
        res(:, 2:5) = expon_pdf(x2, lambda)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol),            &
            msg="exponential_distribution_pdf_rdp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        x1 = expon_rvs(loc, scale)
        x2 = reshape(expon_rvs(loc, scale, 12), [3,4])
        res(:,1) = expon_pdf(x1, loc, scale)
        res(:, 2:5) = expon_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol),            &
            msg="exponential_distribution_pdf_rdp failed", warn=warn)

    end subroutine test_expon_pdf_rdp

    subroutine test_expon_pdf_csp

        complex(sp) :: x1, x2(3,4), lambda, loc, scale
        integer :: seed, get
        real(sp) :: res(3,5)
        real(sp), parameter :: ans(15) =                                   &
                             [0.112097715784191810518066563334849515_sp,   &
                              0.112097715784191810518066563334849515_sp,   &
                              0.112097715784191810518066563334849515_sp,   &
                         4.72087485401191174735651518020251204E-0002_sp,   &
                         3.69705018439006691768174449531170720E-0002_sp,   &
                         8.69498969681198520061798177185735738E-0002_sp,   &
                              0.128007654288233028296342302153338001_sp,   &
                         1.13496395875758374774198906169957218E-0002_sp,   &
                              0.294260498264128747413785056084385424_sp,   &
                         4.66169813179250908948018478030960097E-0002_sp,   &
                         2.84438693906889813143446828488861951E-0002_sp,   &
                              0.161859307815385236742977105439660254_sp,   &
                         4.22904796362406579112752522035325397E-0002_sp,   &
                              0.176117981883470250164040199296778089_sp,   &
                              0.107352342201327219885025541854724060_sp]

        print *, "Test exponential_distribution_pdf_csp"
        seed = 123987654

        ! set args
        lambda = (0.3_sp, 1.6_sp)
        loc    = (0._sp, 0._sp)
        scale  = cmplx(1.0_sp/lambda%re, 1.0_sp/lambda%im, kind=sp)

        ! tests using interface for lambda
        call random_seed(seed, get)
        x1 = expon_rvs(lambda)
        x2 = reshape(expon_rvs(lambda, 12), [3,4])
        res(:,1) = expon_pdf(x1, lambda)
        res(:, 2:5) = expon_pdf(x2, lambda)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol),            &
            msg="exponential_distribution_pdf_csp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        x1 = expon_rvs(loc, scale)
        x2 = reshape(expon_rvs(loc, scale, 12), [3,4])
        res(:,1) = expon_pdf(x1, loc, scale)
        res(:, 2:5) = expon_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol),            &
            msg="exponential_distribution_pdf_csp failed", warn=warn)

    end subroutine test_expon_pdf_csp

    subroutine test_expon_pdf_cdp

        complex(dp) :: x1, x2(3,4), lambda, loc, scale
        integer :: seed, get
        real(dp) :: res(3,5)
        real(dp), parameter :: ans(15) =                                   &
                             [0.112097715784191810518066563334849515_dp,   &
                              0.112097715784191810518066563334849515_dp,   &
                              0.112097715784191810518066563334849515_dp,   &
                         4.72087485401191174735651518020251204E-0002_dp,   &
                         3.69705018439006691768174449531170720E-0002_dp,   &
                         8.69498969681198520061798177185735738E-0002_dp,   &
                              0.128007654288233028296342302153338001_dp,   &
                         1.13496395875758374774198906169957218E-0002_dp,   &
                              0.294260498264128747413785056084385424_dp,   &
                         4.66169813179250908948018478030960097E-0002_dp,   &
                         2.84438693906889813143446828488861951E-0002_dp,   &
                              0.161859307815385236742977105439660254_dp,   &
                         4.22904796362406579112752522035325397E-0002_dp,   &
                              0.176117981883470250164040199296778089_dp,   &
                              0.107352342201327219885025541854724060_dp]

        print *, "Test exponential_distribution_pdf_cdp"
        seed = 123987654

        ! set args
        lambda = (0.3_dp, 1.6_dp)
        loc    = (0._dp, 0._dp)
        scale  = cmplx(1.0_dp/lambda%re, 1.0_dp/lambda%im, kind=dp)

        ! tests using interface for lambda
        call random_seed(seed, get)
        x1 = expon_rvs(lambda)
        x2 = reshape(expon_rvs(lambda, 12), [3,4])
        res(:,1) = expon_pdf(x1, lambda)
        res(:, 2:5) = expon_pdf(x2, lambda)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol),            &
            msg="exponential_distribution_pdf_cdp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        x1 = expon_rvs(loc, scale)
        x2 = reshape(expon_rvs(loc, scale, 12), [3,4])
        res(:,1) = expon_pdf(x1, loc, scale)
        res(:, 2:5) = expon_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol),            &
            msg="exponential_distribution_pdf_cdp failed", warn=warn)

    end subroutine test_expon_pdf_cdp





    subroutine test_expon_cdf_rsp

        real(sp) :: x1, x2(3,4), lambda, loc, scale
        integer :: seed, get
        real(sp) :: res(3,5)
        real(sp), parameter :: ans(15) =                                   &
                           [0.109257742653704886153815776449785051_sp,     &
                            0.109257742653704886153815776449785051_sp,     &
                            0.109257742653704886153815776449785051_sp,     &
                            0.717506371795765265795319089684216215_sp,     &
                       6.82471795435370961628021592837348251E-0002_sp,     &
                            0.158022297254037860379992220663140220_sp,     &
                            0.914579543576380160727189390750289231_sp,     &
                            0.735445094339121647068624074363021598_sp,     &
                       8.69845458684957375690771394578441361E-0002_sp,     &
                            0.491195342629961409581199224477971938_sp,     &
                            0.574283568793105916250099261345264380_sp,     &
                            0.312823040527767907760475800138803955_sp,     &
                            0.640029783598040153827956625977856239_sp,     &
                       2.16202116731629451897815202649346917E-0002_sp,     &
                       7.74788145547936974757767867581111655E-0002_sp]

        print *, "Test exponential_distribution_cdf_rsp"
        seed = 621957438

        ! set args
        lambda = 2.0_sp
        loc    = 0._sp
        scale  = 1.0_sp/lambda

        ! tests using interface for lambda
        call random_seed(seed, get)
        x1 = expon_rvs(lambda)
        x2 = reshape(expon_rvs(lambda, 12), [3,4])
        res(:,1) = expon_cdf(x1, lambda)
        res(:, 2:5) = expon_cdf(x2, lambda)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol),             &
            msg="exponential_distribution_cdf_rsp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        x1 = expon_rvs(loc, scale)
        x2 = reshape(expon_rvs(loc, scale, 12), [3,4])
        res(:,1) = expon_cdf(x1, loc, scale)
        res(:, 2:5) = expon_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol),             &
            msg="exponential_distribution_cdf_rsp failed", warn=warn)

    end subroutine test_expon_cdf_rsp

    subroutine test_expon_cdf_rdp

        real(dp) :: x1, x2(3,4), lambda, loc, scale
        integer :: seed, get
        real(dp) :: res(3,5)
        real(dp), parameter :: ans(15) =                                   &
                           [0.109257742653704886153815776449785051_dp,     &
                            0.109257742653704886153815776449785051_dp,     &
                            0.109257742653704886153815776449785051_dp,     &
                            0.717506371795765265795319089684216215_dp,     &
                       6.82471795435370961628021592837348251E-0002_dp,     &
                            0.158022297254037860379992220663140220_dp,     &
                            0.914579543576380160727189390750289231_dp,     &
                            0.735445094339121647068624074363021598_dp,     &
                       8.69845458684957375690771394578441361E-0002_dp,     &
                            0.491195342629961409581199224477971938_dp,     &
                            0.574283568793105916250099261345264380_dp,     &
                            0.312823040527767907760475800138803955_dp,     &
                            0.640029783598040153827956625977856239_dp,     &
                       2.16202116731629451897815202649346917E-0002_dp,     &
                       7.74788145547936974757767867581111655E-0002_dp]

        print *, "Test exponential_distribution_cdf_rdp"
        seed = 621957438

        ! set args
        lambda = 2.0_dp
        loc    = 0._dp
        scale  = 1.0_dp/lambda

        ! tests using interface for lambda
        call random_seed(seed, get)
        x1 = expon_rvs(lambda)
        x2 = reshape(expon_rvs(lambda, 12), [3,4])
        res(:,1) = expon_cdf(x1, lambda)
        res(:, 2:5) = expon_cdf(x2, lambda)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol),             &
            msg="exponential_distribution_cdf_rdp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        x1 = expon_rvs(loc, scale)
        x2 = reshape(expon_rvs(loc, scale, 12), [3,4])
        res(:,1) = expon_cdf(x1, loc, scale)
        res(:, 2:5) = expon_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol),             &
            msg="exponential_distribution_cdf_rdp failed", warn=warn)

    end subroutine test_expon_cdf_rdp

    subroutine test_expon_cdf_csp

        complex(sp) :: x1, x2(3,4), lambda, loc, scale
        integer :: seed, get
        real(sp) :: res(3,5)
        real(sp), parameter :: ans(15) =                                   &
                        [7.83931265220552191922145459533155073E-0002_sp,   &
                         7.83931265220552191922145459533155073E-0002_sp,   &
                         7.83931265220552191922145459533155073E-0002_sp,   &
                         1.07845760925785109085652212151328215E-0002_sp,   &
                              0.672623038706161724678635394849362256_sp,   &
                         4.27264038113873579678831482902258168E-0002_sp,   &
                              0.179649132114996961326498233168917293_sp,   &
                         1.38375793985183014482681114776428612E-0002_sp,   &
                         3.49246365297941076158369468479748612E-0002_sp,   &
                              0.116869945417176368845403154176734792_sp,   &
                              0.468462732010133566674397830557697485_sp,   &
                              0.413506985517976634907329948218002431_sp,   &
                              0.665679674838121942273909342901808398_sp,   &
                              0.223748595107983772617787558595393205_sp,   &
                              0.337722969540396286456937689606849800_sp]

        print *, "Test exponential_distribution_cdf_csp"
        seed = 621957438

        ! set args
        lambda = (1.3_sp, 2.1_sp)
        loc    = (0._sp, 0._sp)
        scale  = cmplx(1.0_sp/lambda%re, 1.0_sp/lambda%im, kind=sp)

        ! tests using interface for lambda
        call random_seed(seed, get)
        x1 = expon_rvs(lambda)
        x2 = reshape(expon_rvs(lambda, 12), [3,4])
        res(:,1) = expon_cdf(x1, lambda)
        res(:, 2:5) = expon_cdf(x2, lambda)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol),             &
            msg="exponential_distribution_cdf_csp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        x1 = expon_rvs(loc, scale)
        x2 = reshape(expon_rvs(loc, scale, 12), [3,4])
        res(:,1) = expon_cdf(x1, loc, scale)
        res(:, 2:5) = expon_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol),             &
            msg="exponential_distribution_cdf_csp failed", warn=warn)

    end subroutine test_expon_cdf_csp

    subroutine test_expon_cdf_cdp

        complex(dp) :: x1, x2(3,4), lambda, loc, scale
        integer :: seed, get
        real(dp) :: res(3,5)
        real(dp), parameter :: ans(15) =                                   &
                        [7.83931265220552191922145459533155073E-0002_dp,   &
                         7.83931265220552191922145459533155073E-0002_dp,   &
                         7.83931265220552191922145459533155073E-0002_dp,   &
                         1.07845760925785109085652212151328215E-0002_dp,   &
                              0.672623038706161724678635394849362256_dp,   &
                         4.27264038113873579678831482902258168E-0002_dp,   &
                              0.179649132114996961326498233168917293_dp,   &
                         1.38375793985183014482681114776428612E-0002_dp,   &
                         3.49246365297941076158369468479748612E-0002_dp,   &
                              0.116869945417176368845403154176734792_dp,   &
                              0.468462732010133566674397830557697485_dp,   &
                              0.413506985517976634907329948218002431_dp,   &
                              0.665679674838121942273909342901808398_dp,   &
                              0.223748595107983772617787558595393205_dp,   &
                              0.337722969540396286456937689606849800_dp]

        print *, "Test exponential_distribution_cdf_cdp"
        seed = 621957438

        ! set args
        lambda = (1.3_dp, 2.1_dp)
        loc    = (0._dp, 0._dp)
        scale  = cmplx(1.0_dp/lambda%re, 1.0_dp/lambda%im, kind=dp)

        ! tests using interface for lambda
        call random_seed(seed, get)
        x1 = expon_rvs(lambda)
        x2 = reshape(expon_rvs(lambda, 12), [3,4])
        res(:,1) = expon_cdf(x1, lambda)
        res(:, 2:5) = expon_cdf(x2, lambda)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol),             &
            msg="exponential_distribution_cdf_cdp failed", warn=warn)

        ! tests using interface for loc and scale
        call random_seed(seed, get)
        x1 = expon_rvs(loc, scale)
        x2 = reshape(expon_rvs(loc, scale, 12), [3,4])
        res(:,1) = expon_cdf(x1, loc, scale)
        res(:, 2:5) = expon_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol),             &
            msg="exponential_distribution_cdf_cdp failed", warn=warn)

    end subroutine test_expon_cdf_cdp


end program test_distribution_expon
