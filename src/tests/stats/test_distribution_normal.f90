program test_distribution_normal
    use stdlib_kinds
    use stdlib_error, only : check
    use stdlib_stats_distribution_PRNG, only : random_seed
    use stdlib_stats_distribution_normal, nor_rvs => normal_distribution_rvs,  &
                                          nor_pdf => normal_distribution_pdf,  &
                                          nor_cdf => normal_distribution_cdf

    implicit none
    real(sp), parameter :: sptol = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1.0_dp)
    real(qp), parameter :: qptol = 1000 * epsilon(1.0_qp)
    logical ::  warn = .true.
    integer :: put, get
    real :: x(2,3,4),a(2,3,4), b(2,3,4)
    complex :: loc, scale

    put = 12345678
    call random_seed(put, get)

    call test_normal_random_generator

    call test_nor_rvs_rsp
    call test_nor_rvs_rdp
    call test_nor_rvs_rqp
    call test_nor_rvs_csp
    call test_nor_rvs_cdp
    call test_nor_rvs_cqp

    call test_nor_pdf_rsp
    call test_nor_pdf_rdp
    call test_nor_pdf_rqp
    call test_nor_pdf_csp
    call test_nor_pdf_cdp
    call test_nor_pdf_cqp

    call test_nor_cdf_rsp
    call test_nor_cdf_rdp
    call test_nor_cdf_rqp
    call test_nor_cdf_csp
    call test_nor_cdf_cdp
    call test_nor_cdf_cqp
    stop


    contains


    subroutine test_normal_random_generator
        integer :: i, j, freq(0:1000), num=10000000
        real(dp) :: chisq, expct

        print *, ""
        print *, "Test normal random generator with chi-squared"
        freq = 0
        do i = 1, num
            j = 1000 * (1 + erf(nor_rvs(0.0, 1.0) / sqrt(2.0))) / 2.0
            freq(j) = freq(j) + 1
        end do
        chisq = 0.0_dp
        expct = num / 1000
        do i = 0, 999
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do
        write(*,*) "The critical values for chi-squared with 1000 d. of f. is"  &
                           //" 1143.92"
        write(*,*) "Chi-squared for normal random generator is : ", chisq
        call check((chisq < 1143.9), &
               msg="normal randomness failed chi-squared test", warn=warn)
    end subroutine test_normal_random_generator


    subroutine test_nor_rvs_rsp
        real(sp) :: res(10), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        real(sp) :: ans(10) = [2.66708039318040679432897377409972250_sp,      &
                             2.36030794936128329730706809641560540_sp,      &
                             1.27712218793084242296487218482070602_sp,      &
                             -2.39132544130814794769435138732660562_sp,     &
                             1.72566595106028652928387145948363468_sp,      &
                             -1.50621775537767632613395107910037041_sp,     &
                             2.13518835158352082714827702147886157_sp,      &
                             -0.636788253742142318358787633769679815_sp,    &
                             2.48600787778845799813609573902795091_sp,      &
                             -3.03711473837981227319460231228731573_sp]

        print *, "Test normal_distribution_rvs_rsp"
        seed = 25836914
        call random_seed(seed, get)

        loc = 0.5_sp; scale = 2.0_sp
        do i = 1, k
           res(i) = nor_rvs(loc, scale)     ! 2 dummies
        end do
        res(6:10) = nor_rvs(loc, scale, k)  ! 3 dummies
        call check(all(abs(res - ans) < sptol), &
            msg="normal_distribution_rvs_rsp failed", warn=warn)
    end subroutine test_nor_rvs_rsp

    subroutine test_nor_rvs_rdp
        real(dp) :: res(10), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        real(dp) :: ans(10) = [2.66708039318040679432897377409972250_dp,      &
                             2.36030794936128329730706809641560540_dp,      &
                             1.27712218793084242296487218482070602_dp,      &
                             -2.39132544130814794769435138732660562_dp,     &
                             1.72566595106028652928387145948363468_dp,      &
                             -1.50621775537767632613395107910037041_dp,     &
                             2.13518835158352082714827702147886157_dp,      &
                             -0.636788253742142318358787633769679815_dp,    &
                             2.48600787778845799813609573902795091_dp,      &
                             -3.03711473837981227319460231228731573_dp]

        print *, "Test normal_distribution_rvs_rdp"
        seed = 25836914
        call random_seed(seed, get)

        loc = 0.5_dp; scale = 2.0_dp
        do i = 1, k
           res(i) = nor_rvs(loc, scale)     ! 2 dummies
        end do
        res(6:10) = nor_rvs(loc, scale, k)  ! 3 dummies
        call check(all(abs(res - ans) < dptol), &
            msg="normal_distribution_rvs_rdp failed", warn=warn)
    end subroutine test_nor_rvs_rdp

    subroutine test_nor_rvs_rqp
        real(qp) :: res(10), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        real(qp) :: ans(10) = [2.66708039318040679432897377409972250_qp,      &
                             2.36030794936128329730706809641560540_qp,      &
                             1.27712218793084242296487218482070602_qp,      &
                             -2.39132544130814794769435138732660562_qp,     &
                             1.72566595106028652928387145948363468_qp,      &
                             -1.50621775537767632613395107910037041_qp,     &
                             2.13518835158352082714827702147886157_qp,      &
                             -0.636788253742142318358787633769679815_qp,    &
                             2.48600787778845799813609573902795091_qp,      &
                             -3.03711473837981227319460231228731573_qp]

        print *, "Test normal_distribution_rvs_rqp"
        seed = 25836914
        call random_seed(seed, get)

        loc = 0.5_qp; scale = 2.0_qp
        do i = 1, k
           res(i) = nor_rvs(loc, scale)     ! 2 dummies
        end do
        res(6:10) = nor_rvs(loc, scale, k)  ! 3 dummies
        call check(all(abs(res - ans) < qptol), &
            msg="normal_distribution_rvs_rqp failed", warn=warn)
    end subroutine test_nor_rvs_rqp

    subroutine test_nor_rvs_csp
        complex(sp) :: res(10), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        complex(sp) :: ans(10) = [(2.12531018257141113281250000000000000_sp,     &
                              1.46507704257965087890625000000000000_sp),    &
                             (1.08284163475036621093750000000000000_sp,     &
                              0.277168631553649902343750000000000000_sp),   &
                             (1.41924941539764404296875000000000000_sp,     &
                              0.498445570468902587890625000000000000_sp),   &
                             (1.72639131546020507812500000000000000_sp,     &
                              0.715802907943725585937500000000000000_sp),   &
                             (1.98950588703155517578125000000000000_sp,     &
                              0.115721315145492553710937500000000000_sp),   &
                             (-1.16929018497467041015625000000000000_sp,    &
                              0.250744730234146118164062500000000000_sp),   &
                             (1.57160544395446777343750000000000000_sp,     &
                              0.638282597064971923828125000000000000_sp),   &
                             (-1.36106109619140625000000000000000000_sp,    &
                              0.166259199380874633789062500000000000_sp),   &
                             (1.13403093814849853515625000000000000_sp,     &
                              1.04232621192932128906250000000000000_sp),    &
                             (-1.68220531940460205078125000000000000_sp,    &
                              1.63361442089080810546875000000000000_sp)]

        print *, "Test normal_distribution_rvs_csp"
        seed = 25836914
        call random_seed(seed, get)

        loc = (0.5_sp, 1.0_sp); scale = (1.5_sp, 0.5_sp)
        do i = 1, k
           res(i) = nor_rvs(loc, scale)     ! 2 dummies
        end do
        res(6:10) = nor_rvs(loc, scale, k)  ! 3 dummies
        call check(all(abs(real(res) - real(ans)) < sptol) .and.            &
                     all(abs(aimag(res) - aimag(ans)) < sptol),             &
            msg="normal_distribution_rvs_csp failed", warn=warn)
    end subroutine test_nor_rvs_csp

    subroutine test_nor_rvs_cdp
        complex(dp) :: res(10), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        complex(dp) :: ans(10) = [(2.12531018257141113281250000000000000_dp,     &
                              1.46507704257965087890625000000000000_dp),    &
                             (1.08284163475036621093750000000000000_dp,     &
                              0.277168631553649902343750000000000000_dp),   &
                             (1.41924941539764404296875000000000000_dp,     &
                              0.498445570468902587890625000000000000_dp),   &
                             (1.72639131546020507812500000000000000_dp,     &
                              0.715802907943725585937500000000000000_dp),   &
                             (1.98950588703155517578125000000000000_dp,     &
                              0.115721315145492553710937500000000000_dp),   &
                             (-1.16929018497467041015625000000000000_dp,    &
                              0.250744730234146118164062500000000000_dp),   &
                             (1.57160544395446777343750000000000000_dp,     &
                              0.638282597064971923828125000000000000_dp),   &
                             (-1.36106109619140625000000000000000000_dp,    &
                              0.166259199380874633789062500000000000_dp),   &
                             (1.13403093814849853515625000000000000_dp,     &
                              1.04232621192932128906250000000000000_dp),    &
                             (-1.68220531940460205078125000000000000_dp,    &
                              1.63361442089080810546875000000000000_dp)]

        print *, "Test normal_distribution_rvs_cdp"
        seed = 25836914
        call random_seed(seed, get)

        loc = (0.5_dp, 1.0_dp); scale = (1.5_dp, 0.5_dp)
        do i = 1, k
           res(i) = nor_rvs(loc, scale)     ! 2 dummies
        end do
        res(6:10) = nor_rvs(loc, scale, k)  ! 3 dummies
        call check(all(abs(real(res) - real(ans)) < dptol) .and.            &
                     all(abs(aimag(res) - aimag(ans)) < dptol),             &
            msg="normal_distribution_rvs_cdp failed", warn=warn)
    end subroutine test_nor_rvs_cdp

    subroutine test_nor_rvs_cqp
        complex(qp) :: res(10), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        complex(qp) :: ans(10) = [(2.12531018257141113281250000000000000_qp,     &
                              1.46507704257965087890625000000000000_qp),    &
                             (1.08284163475036621093750000000000000_qp,     &
                              0.277168631553649902343750000000000000_qp),   &
                             (1.41924941539764404296875000000000000_qp,     &
                              0.498445570468902587890625000000000000_qp),   &
                             (1.72639131546020507812500000000000000_qp,     &
                              0.715802907943725585937500000000000000_qp),   &
                             (1.98950588703155517578125000000000000_qp,     &
                              0.115721315145492553710937500000000000_qp),   &
                             (-1.16929018497467041015625000000000000_qp,    &
                              0.250744730234146118164062500000000000_qp),   &
                             (1.57160544395446777343750000000000000_qp,     &
                              0.638282597064971923828125000000000000_qp),   &
                             (-1.36106109619140625000000000000000000_qp,    &
                              0.166259199380874633789062500000000000_qp),   &
                             (1.13403093814849853515625000000000000_qp,     &
                              1.04232621192932128906250000000000000_qp),    &
                             (-1.68220531940460205078125000000000000_qp,    &
                              1.63361442089080810546875000000000000_qp)]

        print *, "Test normal_distribution_rvs_cqp"
        seed = 25836914
        call random_seed(seed, get)

        loc = (0.5_qp, 1.0_qp); scale = (1.5_qp, 0.5_qp)
        do i = 1, k
           res(i) = nor_rvs(loc, scale)     ! 2 dummies
        end do
        res(6:10) = nor_rvs(loc, scale, k)  ! 3 dummies
        call check(all(abs(real(res) - real(ans)) < qptol) .and.            &
                     all(abs(aimag(res) - aimag(ans)) < qptol),             &
            msg="normal_distribution_rvs_cqp failed", warn=warn)
    end subroutine test_nor_rvs_cqp



    subroutine test_nor_pdf_rsp
        real(sp) :: x1, x2(3,4), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [0.215050772, 0.215050772, 0.215050772, 0.200537622,  &
                           5.66161536E-02, 0.238986954, 0.265935957,0.262147546,&
                           0.249866411, 3.98721099E-02, 0.265902370,0.161311597,&
                           0.249177739, 0.237427220, 0.155696079]

        print *, "Test normal_distribution_pdf_rsp"
        seed = 741852963
        call random_seed(seed, get)

        loc = -0.5_sp; scale = 1.5_sp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="normal_distribution_pdf_rsp failed", warn=warn)
    end subroutine test_nor_pdf_rsp

    subroutine test_nor_pdf_rdp
        real(dp) :: x1, x2(3,4), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [0.215050772, 0.215050772, 0.215050772, 0.200537622,  &
                           5.66161536E-02, 0.238986954, 0.265935957,0.262147546,&
                           0.249866411, 3.98721099E-02, 0.265902370,0.161311597,&
                           0.249177739, 0.237427220, 0.155696079]

        print *, "Test normal_distribution_pdf_rdp"
        seed = 741852963
        call random_seed(seed, get)

        loc = -0.5_dp; scale = 1.5_dp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol), &
            msg="normal_distribution_pdf_rdp failed", warn=warn)
    end subroutine test_nor_pdf_rdp

    subroutine test_nor_pdf_rqp
        real(qp) :: x1, x2(3,4), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [0.215050772, 0.215050772, 0.215050772, 0.200537622,  &
                           5.66161536E-02, 0.238986954, 0.265935957,0.262147546,&
                           0.249866411, 3.98721099E-02, 0.265902370,0.161311597,&
                           0.249177739, 0.237427220, 0.155696079]

        print *, "Test normal_distribution_pdf_rqp"
        seed = 741852963
        call random_seed(seed, get)

        loc = -0.5_qp; scale = 1.5_qp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < qptol), &
            msg="normal_distribution_pdf_rqp failed", warn=warn)
    end subroutine test_nor_pdf_rqp

    subroutine test_nor_pdf_csp
        complex(sp) :: x1, x2(3,4), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [0.129377306, 0.129377306, 0.129377306,4.05915640E-02,&
                           0.209143385, 2.98881028E-02,0.128679410, 0.177484736,&
                           3.82205285E-02, 7.09915683E-02, 4.56126593E-02,      &
                           6.57454208E-02,0.165161029,3.86104845E-02,0.196922958]

        print *, "Test normal_distribution_pdf_csp"
        seed = 741852963
        call random_seed(seed, get)

        loc = (-0.5_sp, 0.5_sp); scale = (0.5_sp, 1.5_sp)
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="normal_distribution_pdf_csp failed", warn=warn)
    end subroutine test_nor_pdf_csp

    subroutine test_nor_pdf_cdp
        complex(dp) :: x1, x2(3,4), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [0.129377306, 0.129377306, 0.129377306,4.05915640E-02,&
                           0.209143385, 2.98881028E-02,0.128679410, 0.177484736,&
                           3.82205285E-02, 7.09915683E-02, 4.56126593E-02,      &
                           6.57454208E-02,0.165161029,3.86104845E-02,0.196922958]

        print *, "Test normal_distribution_pdf_cdp"
        seed = 741852963
        call random_seed(seed, get)

        loc = (-0.5_dp, 0.5_dp); scale = (0.5_dp, 1.5_dp)
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol), &
            msg="normal_distribution_pdf_cdp failed", warn=warn)
    end subroutine test_nor_pdf_cdp

    subroutine test_nor_pdf_cqp
        complex(qp) :: x1, x2(3,4), loc, scale
        integer :: i, n, k = 5
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [0.129377306, 0.129377306, 0.129377306,4.05915640E-02,&
                           0.209143385, 2.98881028E-02,0.128679410, 0.177484736,&
                           3.82205285E-02, 7.09915683E-02, 4.56126593E-02,      &
                           6.57454208E-02,0.165161029,3.86104845E-02,0.196922958]

        print *, "Test normal_distribution_pdf_cqp"
        seed = 741852963
        call random_seed(seed, get)

        loc = (-0.5_qp, 0.5_qp); scale = (0.5_qp, 1.5_qp)
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < qptol), &
            msg="normal_distribution_pdf_cqp failed", warn=warn)
    end subroutine test_nor_pdf_cqp


    subroutine test_nor_cdf_rsp
        real(sp) :: x1, x2(3,4), loc, scale
        integer :: i, n
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [7.50826299E-02, 7.50826299E-02, 7.50826299E-02,      &
                           0.143119827, 0.241425425, 0.284345865, 0.233239830,  &
                          0.341059506,0.353156865,6.81066737E-02,4.38792333E-02,&
                           0.763679624, 0.363722175, 0.868187129, 0.626506805]

        print *, "Test normal_distribution_cdf_rsp"
        seed = 369147582
        call random_seed(seed, get)

        loc = -1.0_sp; scale = 2.0_sp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="normal_distribution_cdf_rsp failed", warn=warn)
    end subroutine test_nor_cdf_rsp

    subroutine test_nor_cdf_rdp
        real(dp) :: x1, x2(3,4), loc, scale
        integer :: i, n
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [7.50826299E-02, 7.50826299E-02, 7.50826299E-02,      &
                           0.143119827, 0.241425425, 0.284345865, 0.233239830,  &
                          0.341059506,0.353156865,6.81066737E-02,4.38792333E-02,&
                           0.763679624, 0.363722175, 0.868187129, 0.626506805]

        print *, "Test normal_distribution_cdf_rdp"
        seed = 369147582
        call random_seed(seed, get)

        loc = -1.0_dp; scale = 2.0_dp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol), &
            msg="normal_distribution_cdf_rdp failed", warn=warn)
    end subroutine test_nor_cdf_rdp

    subroutine test_nor_cdf_rqp
        real(qp) :: x1, x2(3,4), loc, scale
        integer :: i, n
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [7.50826299E-02, 7.50826299E-02, 7.50826299E-02,      &
                           0.143119827, 0.241425425, 0.284345865, 0.233239830,  &
                          0.341059506,0.353156865,6.81066737E-02,4.38792333E-02,&
                           0.763679624, 0.363722175, 0.868187129, 0.626506805]

        print *, "Test normal_distribution_cdf_rqp"
        seed = 369147582
        call random_seed(seed, get)

        loc = -1.0_qp; scale = 2.0_qp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < qptol), &
            msg="normal_distribution_cdf_rqp failed", warn=warn)
    end subroutine test_nor_cdf_rqp

    subroutine test_nor_cdf_csp
        complex(sp) :: x1, x2(3,4), loc, scale
        integer :: i, n
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [1.07458141E-02, 1.07458141E-02, 1.07458141E-02,      &
                           6.86483160E-02, 7.95486569E-02, 2.40523368E-02,      &
                           3.35096717E-02, 0.315778911,0.446311295,0.102010213, &
                           7.66918957E-02, 0.564691007, 0.708769500,            &
                           6.40553832E-02, 5.39999120E-02]

        print *, "Test normal_distribution_cdf_csp"
        seed = 369147582
        call random_seed(seed, get)

        loc = (-1.0_sp, 1.0_sp); scale = (1.7_sp, 2.4_sp)
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="normal_distribution_cdf_csp failed", warn=warn)
    end subroutine test_nor_cdf_csp

    subroutine test_nor_cdf_cdp
        complex(dp) :: x1, x2(3,4), loc, scale
        integer :: i, n
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [1.07458141E-02, 1.07458141E-02, 1.07458141E-02,      &
                           6.86483160E-02, 7.95486569E-02, 2.40523368E-02,      &
                           3.35096717E-02, 0.315778911,0.446311295,0.102010213, &
                           7.66918957E-02, 0.564691007, 0.708769500,            &
                           6.40553832E-02, 5.39999120E-02]

        print *, "Test normal_distribution_cdf_cdp"
        seed = 369147582
        call random_seed(seed, get)

        loc = (-1.0_dp, 1.0_dp); scale = (1.7_dp, 2.4_dp)
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol), &
            msg="normal_distribution_cdf_cdp failed", warn=warn)
    end subroutine test_nor_cdf_cdp

    subroutine test_nor_cdf_cqp
        complex(qp) :: x1, x2(3,4), loc, scale
        integer :: i, n
        integer :: seed, get
        real :: res(3,5)
        real :: ans(15) = [1.07458141E-02, 1.07458141E-02, 1.07458141E-02,      &
                           6.86483160E-02, 7.95486569E-02, 2.40523368E-02,      &
                           3.35096717E-02, 0.315778911,0.446311295,0.102010213, &
                           7.66918957E-02, 0.564691007, 0.708769500,            &
                           6.40553832E-02, 5.39999120E-02]

        print *, "Test normal_distribution_cdf_cqp"
        seed = 369147582
        call random_seed(seed, get)

        loc = (-1.0_qp, 1.0_qp); scale = (1.7_qp, 2.4_qp)
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < qptol), &
            msg="normal_distribution_cdf_cqp failed", warn=warn)
    end subroutine test_nor_cdf_cqp


end program test_distribution_normal