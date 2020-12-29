program test_distribution_beta
    use stdlib_kinds
    use stdlib_error, only : check
    use stdlib_stats_distribution_PRNG, only : random_seed
    use stdlib_stats_distribution_beta, beta_rvs => beta_distribution_rvs,      &
                                        beta_pdf => beta_distribution_pdf,      &
                                        beta_cdf => beta_distribution_cdf

    implicit none
    real(sp), parameter :: sptol = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1.0_dp)
    real(qp), parameter :: qptol = 1000 * epsilon(1.0_qp)
    logical ::  warn = .true.
    integer :: put, get
	
    real ::  aa(2,3,4), bb(2,3,4), x(2,3,4)
    complex :: a, b

    put = 1234567
    call random_seed(put, get)
    print *, beta_cdf(0.4, 0.5,1.0)
    ! a standard beta cumulative at 0.4 with a=0.5, b=1.0

! 0.842700839

    print *, beta_cdf(0.8, 1.5,2.0)
    ! a cumulative at 0.8 with a a=1.5, b=2.0

! 0.953988254

    aa(:,:,:) = 2.0
    bb(:,:,:) = 3.0
    x = reshape(beta_rvs(2.0, 3.0, 24),[2,3,4])
    !beta random variates array with a a=2.0, b=3.0
    print *, beta_cdf(x,aa,bb)        ! a rank 3 standard beta cumulative array

! [0.710880339, 0.472411335, 0.578345954, 0.383050948, 0.870905757,
!  0.870430350, 0.170215249, 0.677347481, 0.620089889, 0.161825046,
!  4.17549349E-02, 0.510665894, 0.252201647, 0.911497891, 0.984424412,
!  0.635621786, 0.177783430, 0.414842933, 0.871342421, 0.338317066,
!  2.06879266E-02, 0.335232288, 0.907408893, 0.624871135]

    a = (.7, 2.1)
    b = (0.5,1.0)
    print *, beta_cdf((0.5,0.5),a,b)
stop
    put = 1234567
    call random_seed(put, get)

    call test_beta_random_generator

    call test_beta_rvs_rsp
    call test_beta_rvs_rdp
    call test_beta_rvs_rqp
    call test_beta_rvs_csp
    call test_beta_rvs_cdp
    call test_beta_rvs_cqp

    call test_beta_pdf_rsp
    call test_beta_pdf_rdp
    call test_beta_pdf_rqp
    call test_beta_pdf_csp
    call test_beta_pdf_cdp
    call test_beta_pdf_cqp

    call test_beta_cdf_rsp
    call test_beta_cdf_rdp
    call test_beta_cdf_rqp
    call test_beta_cdf_csp
    call test_beta_cdf_cdp
    call test_beta_cdf_cqp
    stop

    contains

    subroutine test_beta_random_generator
        integer :: i, j, freq(0:999), num=1000000
        real(dp) :: chisq, expct

        print *, ""
        print *, "Test beta random generator with chi-squared"
        freq = 0
        do i = 1, num
            j = 1000 * beta_cdf(beta_rvs(2.0,1.5),2.0,1.5)
            freq(j) = freq(j) + 1
        end do
        chisq = 0.0_dp
        expct = num / 1000
        do i = 0, 999
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do
        write(*,*) "The critical values for chi-squared with 1000 d. of f. is"  &
                   //" 1143.92"
        write(*,*) "Chi-squared for beta random generator is : ", chisq
        call check((chisq < 1143.9),                                            &
               msg="beta randomness failed chi-squared test", warn=warn)
    end subroutine test_beta_random_generator

    subroutine test_beta_rvs_rsp
        real(sp) :: res(10), a, b
        integer :: i, n, k = 5
        integer :: put, get
        real(sp) :: ans(10) = [0.744399697952416243334363353017662363_sp,     &
                             0.785582064888561104409969900319307115_sp,     &
                             0.290228167791285215460609614976244262_sp,     &
                             0.540957122824810300186112642749043673_sp,     &
                             0.866783498753591906081043187617603418_sp,     &
                             0.164859290722944895746841956125886140_sp,     &
                             0.752018475270934089892015814754187410_sp,     &
                             0.535463312713066631219371237676531884_sp,     &
                             0.438125081488452966935618841567308566_sp,     &
                             0.635255468090749026953184924665020348_sp]

        print *, "Test beta_distribution_rvs_rsp"
        put = 639741825
        call random_seed(put, get)
        a = 2.0_sp; b = 1.0_sp
        do i = 1, 5
            res(i) = beta_rvs(a, b)
        end do
        res(6:10) = beta_rvs(a, b, k)
        call check(all(abs(res - ans) < sptol), &
            msg="beta_distribution_rvs_rsp failed", warn=warn)
    end subroutine test_beta_rvs_rsp

    subroutine test_beta_rvs_rdp
        real(dp) :: res(10), a, b
        integer :: i, n, k = 5
        integer :: put, get
        real(dp) :: ans(10) = [0.744399697952416243334363353017662363_dp,     &
                             0.785582064888561104409969900319307115_dp,     &
                             0.290228167791285215460609614976244262_dp,     &
                             0.540957122824810300186112642749043673_dp,     &
                             0.866783498753591906081043187617603418_dp,     &
                             0.164859290722944895746841956125886140_dp,     &
                             0.752018475270934089892015814754187410_dp,     &
                             0.535463312713066631219371237676531884_dp,     &
                             0.438125081488452966935618841567308566_dp,     &
                             0.635255468090749026953184924665020348_dp]

        print *, "Test beta_distribution_rvs_rdp"
        put = 639741825
        call random_seed(put, get)
        a = 2.0_dp; b = 1.0_dp
        do i = 1, 5
            res(i) = beta_rvs(a, b)
        end do
        res(6:10) = beta_rvs(a, b, k)
        call check(all(abs(res - ans) < dptol), &
            msg="beta_distribution_rvs_rdp failed", warn=warn)
    end subroutine test_beta_rvs_rdp

    subroutine test_beta_rvs_rqp
        real(qp) :: res(10), a, b
        integer :: i, n, k = 5
        integer :: put, get
        real(qp) :: ans(10) = [0.744399697952416243334363353017662363_qp,     &
                             0.785582064888561104409969900319307115_qp,     &
                             0.290228167791285215460609614976244262_qp,     &
                             0.540957122824810300186112642749043673_qp,     &
                             0.866783498753591906081043187617603418_qp,     &
                             0.164859290722944895746841956125886140_qp,     &
                             0.752018475270934089892015814754187410_qp,     &
                             0.535463312713066631219371237676531884_qp,     &
                             0.438125081488452966935618841567308566_qp,     &
                             0.635255468090749026953184924665020348_qp]

        print *, "Test beta_distribution_rvs_rqp"
        put = 639741825
        call random_seed(put, get)
        a = 2.0_qp; b = 1.0_qp
        do i = 1, 5
            res(i) = beta_rvs(a, b)
        end do
        res(6:10) = beta_rvs(a, b, k)
        call check(all(abs(res - ans) < qptol), &
            msg="beta_distribution_rvs_rqp failed", warn=warn)
    end subroutine test_beta_rvs_rqp

    subroutine test_beta_rvs_csp
        complex(sp) :: res(10), a, b
        integer :: i, n, k = 5
        integer :: put, get
        complex(sp) :: ans(10) = [(0.996593894945797558163416198727906880_sp,    &
                              0.132598000233262366166275960014792683_sp),   &
                             (0.390100279193128267998817589162392551_sp,    &
                              0.594960539319605102054215589048597662_sp),   &
                             (0.839296137442072004654073615963339625_sp,    &
                              0.811403350202500212373591232224849046_sp),   &
                             (0.915863048173886740665234455471312086_sp,    &
                              0.791004162831226427993329892479385152_sp),   &
                             (0.449544461366363638651136427537418482_sp,    &
                        6.648931970435189824360854746280901949E-0002_sp),   &
                             (0.418599563122841869588123385796545717_sp,    &
                        2.682053757872834248817007196452821345E-0002_sp),   &
                             (0.847048136644577210744803917816801455_sp,    &
                              0.728350780933130458744978339193529096_sp),   &
                             (0.859055679164195250196327014741975834_sp,    &
                              0.677632443230547158536307254984227248_sp),   &
                             (0.251814018668272502730175719600969280_sp,    &
                        6.410086007672458486925924399711432844E-0003_sp),   &
                             (0.656763996944608477801486502669830627_sp,    &
                              0.870919913077248989108181221896598931_sp)]

        print *, "Test beta_distribution_rvs_csp"
        put = 639741825
        call random_seed(put, get)
        a = (2.0_sp, 0.7_sp); b = (0.8_sp, 1.2_sp)
        do i = 1, 5
            res(i) = beta_rvs(a, b)
        end do
        res(6:10) = beta_rvs(a, b, k)
        call check(all(abs(res - ans) < sptol), &
            msg="beta_distribution_rvs_csp failed", warn=warn)
    end subroutine test_beta_rvs_csp

    subroutine test_beta_rvs_cdp
        complex(dp) :: res(10), a, b
        integer :: i, n, k = 5
        integer :: put, get
        complex(dp) :: ans(10) = [(0.996593894945797558163416198727906880_dp,    &
                              0.132598000233262366166275960014792683_dp),   &
                             (0.390100279193128267998817589162392551_dp,    &
                              0.594960539319605102054215589048597662_dp),   &
                             (0.839296137442072004654073615963339625_dp,    &
                              0.811403350202500212373591232224849046_dp),   &
                             (0.915863048173886740665234455471312086_dp,    &
                              0.791004162831226427993329892479385152_dp),   &
                             (0.449544461366363638651136427537418482_dp,    &
                        6.648931970435189824360854746280901949E-0002_dp),   &
                             (0.418599563122841869588123385796545717_dp,    &
                        2.682053757872834248817007196452821345E-0002_dp),   &
                             (0.847048136644577210744803917816801455_dp,    &
                              0.728350780933130458744978339193529096_dp),   &
                             (0.859055679164195250196327014741975834_dp,    &
                              0.677632443230547158536307254984227248_dp),   &
                             (0.251814018668272502730175719600969280_dp,    &
                        6.410086007672458486925924399711432844E-0003_dp),   &
                             (0.656763996944608477801486502669830627_dp,    &
                              0.870919913077248989108181221896598931_dp)]

        print *, "Test beta_distribution_rvs_cdp"
        put = 639741825
        call random_seed(put, get)
        a = (2.0_dp, 0.7_dp); b = (0.8_dp, 1.2_dp)
        do i = 1, 5
            res(i) = beta_rvs(a, b)
        end do
        res(6:10) = beta_rvs(a, b, k)
        call check(all(abs(res - ans) < dptol), &
            msg="beta_distribution_rvs_cdp failed", warn=warn)
    end subroutine test_beta_rvs_cdp

    subroutine test_beta_rvs_cqp
        complex(qp) :: res(10), a, b
        integer :: i, n, k = 5
        integer :: put, get
        complex(qp) :: ans(10) = [(0.996593894945797558163416198727906880_qp,    &
                              0.132598000233262366166275960014792683_qp),   &
                             (0.390100279193128267998817589162392551_qp,    &
                              0.594960539319605102054215589048597662_qp),   &
                             (0.839296137442072004654073615963339625_qp,    &
                              0.811403350202500212373591232224849046_qp),   &
                             (0.915863048173886740665234455471312086_qp,    &
                              0.791004162831226427993329892479385152_qp),   &
                             (0.449544461366363638651136427537418482_qp,    &
                        6.648931970435189824360854746280901949E-0002_qp),   &
                             (0.418599563122841869588123385796545717_qp,    &
                        2.682053757872834248817007196452821345E-0002_qp),   &
                             (0.847048136644577210744803917816801455_qp,    &
                              0.728350780933130458744978339193529096_qp),   &
                             (0.859055679164195250196327014741975834_qp,    &
                              0.677632443230547158536307254984227248_qp),   &
                             (0.251814018668272502730175719600969280_qp,    &
                        6.410086007672458486925924399711432844E-0003_qp),   &
                             (0.656763996944608477801486502669830627_qp,    &
                              0.870919913077248989108181221896598931_qp)]

        print *, "Test beta_distribution_rvs_cqp"
        put = 639741825
        call random_seed(put, get)
        a = (2.0_qp, 0.7_qp); b = (0.8_qp, 1.2_qp)
        do i = 1, 5
            res(i) = beta_rvs(a, b)
        end do
        res(6:10) = beta_rvs(a, b, k)
        call check(all(abs(res - ans) < qptol), &
            msg="beta_distribution_rvs_cqp failed", warn=warn)
    end subroutine test_beta_rvs_cqp



    subroutine test_beta_pdf_rsp
        real(sp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [1.97584832, 1.97584832, 1.97584832, 1.94792151,      &
                           1.05610514, 1.63085556, 1.44469929, 1.78598392,      &
                           1.03530371, 1.32163048, 1.95935822, 1.49064910,      &
                           1.22708333, 0.816426575, 1.93443334]

        print *, "Test beta_distribution_pdf_rsp"
        put = 345987126
        call random_seed(put, get)
        a = 2.0_sp; b = 1.0_sp
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_pdf(x1, a, b)
        res(:, 2:5) = beta_pdf(x2, a, b)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="beta_distribution_pdf_rsp failed", warn=warn)
    end subroutine test_beta_pdf_rsp

    subroutine test_beta_pdf_rdp
        real(dp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [1.97584832, 1.97584832, 1.97584832, 1.94792151,      &
                           1.05610514, 1.63085556, 1.44469929, 1.78598392,      &
                           1.03530371, 1.32163048, 1.95935822, 1.49064910,      &
                           1.22708333, 0.816426575, 1.93443334]

        print *, "Test beta_distribution_pdf_rdp"
        put = 345987126
        call random_seed(put, get)
        a = 2.0_dp; b = 1.0_dp
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_pdf(x1, a, b)
        res(:, 2:5) = beta_pdf(x2, a, b)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol), &
            msg="beta_distribution_pdf_rdp failed", warn=warn)
    end subroutine test_beta_pdf_rdp

    subroutine test_beta_pdf_rqp
        real(qp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [1.97584832, 1.97584832, 1.97584832, 1.94792151,      &
                           1.05610514, 1.63085556, 1.44469929, 1.78598392,      &
                           1.03530371, 1.32163048, 1.95935822, 1.49064910,      &
                           1.22708333, 0.816426575, 1.93443334]

        print *, "Test beta_distribution_pdf_rqp"
        put = 345987126
        call random_seed(put, get)
        a = 2.0_qp; b = 1.0_qp
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_pdf(x1, a, b)
        res(:, 2:5) = beta_pdf(x2, a, b)
        call check(all(abs(res - reshape(ans, [3,5])) < qptol), &
            msg="beta_distribution_pdf_rqp failed", warn=warn)
    end subroutine test_beta_pdf_rqp

    subroutine test_beta_pdf_csp
        complex(sp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [2.79032898, 2.79032898, 2.79032898, 0.831092536,     &
                           0.859560609, 0.833086491, 2.18498087, 0.611756265,   &
                           5.17011976, 0.805453956, 2.12247658, 0.969030082,    &
                           1.92922175, 0.700230777, 6.4548239]

        print *, "Test beta_distribution_pdf_csp"
        put = 345987126
        call random_seed(put, get)
        a = (2.0_sp, 0.7_sp); b = (0.8_sp, 1.2_sp)
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_pdf(x1, a, b)
        res(:, 2:5) = beta_pdf(x2, a, b)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="beta_distribution_pdf_csp failed", warn=warn)
    end subroutine test_beta_pdf_csp

    subroutine test_beta_pdf_cdp
        complex(dp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [2.79032898, 2.79032898, 2.79032898, 0.831092536,     &
                           0.859560609, 0.833086491, 2.18498087, 0.611756265,   &
                           5.17011976, 0.805453956, 2.12247658, 0.969030082,    &
                           1.92922175, 0.700230777, 6.4548239]

        print *, "Test beta_distribution_pdf_cdp"
        put = 345987126
        call random_seed(put, get)
        a = (2.0_dp, 0.7_dp); b = (0.8_dp, 1.2_dp)
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_pdf(x1, a, b)
        res(:, 2:5) = beta_pdf(x2, a, b)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol), &
            msg="beta_distribution_pdf_cdp failed", warn=warn)
    end subroutine test_beta_pdf_cdp

    subroutine test_beta_pdf_cqp
        complex(qp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [2.79032898, 2.79032898, 2.79032898, 0.831092536,     &
                           0.859560609, 0.833086491, 2.18498087, 0.611756265,   &
                           5.17011976, 0.805453956, 2.12247658, 0.969030082,    &
                           1.92922175, 0.700230777, 6.4548239]

        print *, "Test beta_distribution_pdf_cqp"
        put = 345987126
        call random_seed(put, get)
        a = (2.0_qp, 0.7_qp); b = (0.8_qp, 1.2_qp)
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_pdf(x1, a, b)
        res(:, 2:5) = beta_pdf(x2, a, b)
        call check(all(abs(res - reshape(ans, [3,5])) < qptol), &
            msg="beta_distribution_pdf_cqp failed", warn=warn)
    end subroutine test_beta_pdf_cqp


    subroutine test_beta_cdf_rsp
        real(sp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [0.153344765, 0.153344765, 0.153344765, 0.639326215,  &
                           0.227737889, 0.832331538, 0.215463713, 0.609950244,  &
                           0.552298367, 0.936580479, 0.473157555, 0.375768840,  &
                           2.33022049E-02, 0.907276988, 0.230596066]

        print *, "Test beta_distribution_cdf_rsp"
        put = 567985123
        call random_seed(put, get)
        a = 2.0_sp; b = 2.0_sp
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_cdf(x1, a, b)
        res(:, 2:5) = beta_cdf(x2, a, b)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="beta_distribution_cdf_rsp failed", warn=warn)
    end subroutine test_beta_cdf_rsp

    subroutine test_beta_cdf_rdp
        real(dp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [0.153344765, 0.153344765, 0.153344765, 0.639326215,  &
                           0.227737889, 0.832331538, 0.215463713, 0.609950244,  &
                           0.552298367, 0.936580479, 0.473157555, 0.375768840,  &
                           2.33022049E-02, 0.907276988, 0.230596066]

        print *, "Test beta_distribution_cdf_rdp"
        put = 567985123
        call random_seed(put, get)
        a = 2.0_dp; b = 2.0_dp
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_cdf(x1, a, b)
        res(:, 2:5) = beta_cdf(x2, a, b)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol), &
            msg="beta_distribution_cdf_rdp failed", warn=warn)
    end subroutine test_beta_cdf_rdp

    subroutine test_beta_cdf_rqp
        real(qp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [0.153344765, 0.153344765, 0.153344765, 0.639326215,  &
                           0.227737889, 0.832331538, 0.215463713, 0.609950244,  &
                           0.552298367, 0.936580479, 0.473157555, 0.375768840,  &
                           2.33022049E-02, 0.907276988, 0.230596066]

        print *, "Test beta_distribution_cdf_rqp"
        put = 567985123
        call random_seed(put, get)
        a = 2.0_qp; b = 2.0_qp
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_cdf(x1, a, b)
        res(:, 2:5) = beta_cdf(x2, a, b)
        call check(all(abs(res - reshape(ans,[3,5])) < qptol), &
            msg="beta_distribution_cdf_rqp failed", warn=warn)
    end subroutine test_beta_cdf_rqp

    subroutine test_beta_cdf_csp
        complex(sp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [7.06288144E-02, 7.06288144E-02, 7.06288144E-02,      &
                           3.14221904E-02, 0.229956210, 4.80622090E-02,         &
                           0.495213449, 0.541507423, 0.105880715, 0.194856107,  &
                           3.40392105E-02, 1.09316744E-02, 0.180127904,         &
                           0.654031873, 0.406583667]

        print *, "Test beta_distribution_cdf_csp"
        put = 567985123
        call random_seed(put, get)
        a = (2.0_sp, 0.7_sp); b = (0.8_sp, 1.2_sp)
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_cdf(x1, a, b)
        res(:, 2:5) = beta_cdf(x2, a, b)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="beta_distribution_cdf_csp failed", warn=warn)
    end subroutine test_beta_cdf_csp

    subroutine test_beta_cdf_cdp
        complex(dp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [7.06288144E-02, 7.06288144E-02, 7.06288144E-02,      &
                           3.14221904E-02, 0.229956210, 4.80622090E-02,         &
                           0.495213449, 0.541507423, 0.105880715, 0.194856107,  &
                           3.40392105E-02, 1.09316744E-02, 0.180127904,         &
                           0.654031873, 0.406583667]

        print *, "Test beta_distribution_cdf_cdp"
        put = 567985123
        call random_seed(put, get)
        a = (2.0_dp, 0.7_dp); b = (0.8_dp, 1.2_dp)
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_cdf(x1, a, b)
        res(:, 2:5) = beta_cdf(x2, a, b)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol), &
            msg="beta_distribution_cdf_cdp failed", warn=warn)
    end subroutine test_beta_cdf_cdp

    subroutine test_beta_cdf_cqp
        complex(qp) :: x1, x2(3,4), a, b
        real :: res(3,5)
        integer :: i, n
        integer :: put, get
        real :: ans(15) = [7.06288144E-02, 7.06288144E-02, 7.06288144E-02,      &
                           3.14221904E-02, 0.229956210, 4.80622090E-02,         &
                           0.495213449, 0.541507423, 0.105880715, 0.194856107,  &
                           3.40392105E-02, 1.09316744E-02, 0.180127904,         &
                           0.654031873, 0.406583667]

        print *, "Test beta_distribution_cdf_cqp"
        put = 567985123
        call random_seed(put, get)
        a = (2.0_qp, 0.7_qp); b = (0.8_qp, 1.2_qp)
        x1 = beta_rvs(a, b)
        x2 = reshape(beta_rvs(a, b, 12), [3,4])
        res(:,1) = beta_cdf(x1, a, b)
        res(:, 2:5) = beta_cdf(x2, a, b)
        call check(all(abs(res - reshape(ans,[3,5])) < qptol), &
            msg="beta_distribution_cdf_cqp failed", warn=warn)
    end subroutine test_beta_cdf_cqp


end program test_distribution_beta