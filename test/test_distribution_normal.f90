
program test_distribution_normal
    use stdlib_kinds, only : sp, dp, xdp, qp
    use stdlib_error, only : check
    use stdlib_random, only : random_seed
    use stdlib_stats_distribution_uniform, only : uni => rvs_uniform
    use stdlib_stats_distribution_normal, only : nor_rvs => rvs_normal,        &
                          nor_pdf => pdf_normal, nor_cdf => cdf_normal

    implicit none
    real(sp), parameter :: sptol = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1.0_dp)
    logical ::  warn = .true.
    integer :: put, get

    put = 12345678
    call random_seed(put, get)

    call test_normal_random_generator


    call test_nor_rvs_rsp
    call test_nor_rvs_rdp
    call test_nor_rvs_csp
    call test_nor_rvs_cdp



    call test_nor_pdf_rsp
    call test_nor_pdf_rdp
    call test_nor_pdf_csp
    call test_nor_pdf_cdp



    call test_nor_cdf_rsp
    call test_nor_cdf_rdp
    call test_nor_cdf_csp
    call test_nor_cdf_cdp





contains

    subroutine test_normal_random_generator

        integer, parameter :: num = 10000000, array_size = 1000
        integer :: i, j, freq(0:array_size)
        real(dp) :: chisq, expct

        print *, ""
        print *, "Test normal random generator with chi-squared"
        freq = 0
        do i = 1, num
            j = array_size * (1 + erf(nor_rvs(0.0, 1.0) / sqrt(2.0))) / 2.0
            freq(j) = freq(j) + 1
        end do
        chisq = 0.0_dp
        expct = num / array_size
        do i = 0, array_size - 1
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do
        write(*,*) "The critical values for chi-squared with 1000 d. of f. is" &
            //" 1143.92"
        write(*,*) "Chi-squared for normal random generator is : ", chisq
        call check((chisq < 1143.9),                                           &
            msg = "normal randomness failed chi-squared test", warn = warn)
    end subroutine test_normal_random_generator




    subroutine test_nor_rvs_rsp
        real(sp) :: res(10), loc, scale
        integer, parameter :: k = 5
        integer :: i
        integer :: seed, get
        real(sp), parameter :: ans(10) =                                         &
                            [2.66708039318040679432897377409972250_sp,     &
                             2.36030794936128329730706809641560540_sp,     &
                             1.27712218793084242296487218482070602_sp,     &
                            -2.39132544130814794769435138732660562_sp,     &
                             1.72566595106028652928387145948363468_sp,     &
                            -1.50621775537767632613395107910037041_sp,     &
                             2.13518835158352082714827702147886157_sp,     &
                           -0.636788253742142318358787633769679815_sp,     &
                             2.48600787778845799813609573902795091_sp,     &
                            -3.03711473837981227319460231228731573_sp]

        print *, "Test normal_distribution_rvs_rsp"
        seed = 25836914
        call random_seed(seed, get)
        loc = 0.5_sp; scale = 2.0_sp

        do i = 1, k
           res(i) = nor_rvs(loc, scale)     ! 2 dummies
        end do
        res(6:10) = nor_rvs(loc, scale, k)  ! 3 dummies
        call check(all(abs(res - ans) < sptol),                            &
            msg="normal_distribution_rvs_rsp failed", warn=warn)
    end subroutine test_nor_rvs_rsp

    subroutine test_nor_rvs_rdp
        real(dp) :: res(10), loc, scale
        integer, parameter :: k = 5
        integer :: i
        integer :: seed, get
        real(dp), parameter :: ans(10) =                                         &
                            [2.66708039318040679432897377409972250_dp,     &
                             2.36030794936128329730706809641560540_dp,     &
                             1.27712218793084242296487218482070602_dp,     &
                            -2.39132544130814794769435138732660562_dp,     &
                             1.72566595106028652928387145948363468_dp,     &
                            -1.50621775537767632613395107910037041_dp,     &
                             2.13518835158352082714827702147886157_dp,     &
                           -0.636788253742142318358787633769679815_dp,     &
                             2.48600787778845799813609573902795091_dp,     &
                            -3.03711473837981227319460231228731573_dp]

        print *, "Test normal_distribution_rvs_rdp"
        seed = 25836914
        call random_seed(seed, get)
        loc = 0.5_dp; scale = 2.0_dp

        do i = 1, k
           res(i) = nor_rvs(loc, scale)     ! 2 dummies
        end do
        res(6:10) = nor_rvs(loc, scale, k)  ! 3 dummies
        call check(all(abs(res - ans) < dptol),                            &
            msg="normal_distribution_rvs_rdp failed", warn=warn)
    end subroutine test_nor_rvs_rdp

    subroutine test_nor_rvs_csp
        complex(sp) :: res(10), loc, scale
        integer, parameter :: k = 5
        integer :: i
        integer :: seed, get
        complex(sp), parameter :: ans(10) =                                         &
                            [(2.12531029488530509574673033057479188_sp,    &
                              1.46507698734032082432676702410390135_sp),   &
                             (1.08284164094813181722365413861552952_sp,    &
                             0.277168639672963013076412153168348595_sp),   &
                             (1.41924946329521489696290359461272601_sp,    &
                             0.498445561155580918466512230224907398_sp),   &
                             (1.72639126368764062036120776610914618_sp,    &
                             0.715802936564464420410303091557580046_sp),   &
                             (1.98950590834134349860207180427096318_sp,    &
                             0.115721315405046931701349421928171068_sp),   &
                            (-1.16929014824793620075382705181255005_sp,    &
                             0.250744737486995217246033007540972903_sp),   &
                             (1.57160542831869509683428987045772374_sp,    &
                             0.638282596371312238581197107123443857_sp),   &
                            (-1.36106107654239116833139178197598085_sp,    &
                             0.166259201494369124318950525776017457_sp),   &
                             (1.13403096805387920698038328737311531_sp,    &
                              1.04232618148691447146347854868508875_sp),   &
                            (-1.68220535920475811053620418533682823_sp,    &
                              1.63361446685040256898702182297711261_sp)]

        print *, "Test normal_distribution_rvs_csp"
        seed = 25836914
        call random_seed(seed, get)
        loc = (0.5_sp, 1.0_sp); scale = (1.5_sp, 0.5_sp)

        do i = 1, k
           res(i) = nor_rvs(loc, scale)     ! 2 dummies
        end do
        res(6:10) = nor_rvs(loc, scale, k)  ! 3 dummies
        call check(all(abs(res - ans) < sptol),                            &
            msg="normal_distribution_rvs_csp failed", warn=warn)
    end subroutine test_nor_rvs_csp

    subroutine test_nor_rvs_cdp
        complex(dp) :: res(10), loc, scale
        integer, parameter :: k = 5
        integer :: i
        integer :: seed, get
        complex(dp), parameter :: ans(10) =                                         &
                            [(2.12531029488530509574673033057479188_dp,    &
                              1.46507698734032082432676702410390135_dp),   &
                             (1.08284164094813181722365413861552952_dp,    &
                             0.277168639672963013076412153168348595_dp),   &
                             (1.41924946329521489696290359461272601_dp,    &
                             0.498445561155580918466512230224907398_dp),   &
                             (1.72639126368764062036120776610914618_dp,    &
                             0.715802936564464420410303091557580046_dp),   &
                             (1.98950590834134349860207180427096318_dp,    &
                             0.115721315405046931701349421928171068_dp),   &
                            (-1.16929014824793620075382705181255005_dp,    &
                             0.250744737486995217246033007540972903_dp),   &
                             (1.57160542831869509683428987045772374_dp,    &
                             0.638282596371312238581197107123443857_dp),   &
                            (-1.36106107654239116833139178197598085_dp,    &
                             0.166259201494369124318950525776017457_dp),   &
                             (1.13403096805387920698038328737311531_dp,    &
                              1.04232618148691447146347854868508875_dp),   &
                            (-1.68220535920475811053620418533682823_dp,    &
                              1.63361446685040256898702182297711261_dp)]

        print *, "Test normal_distribution_rvs_cdp"
        seed = 25836914
        call random_seed(seed, get)
        loc = (0.5_dp, 1.0_dp); scale = (1.5_dp, 0.5_dp)

        do i = 1, k
           res(i) = nor_rvs(loc, scale)     ! 2 dummies
        end do
        res(6:10) = nor_rvs(loc, scale, k)  ! 3 dummies
        call check(all(abs(res - ans) < dptol),                            &
            msg="normal_distribution_rvs_cdp failed", warn=warn)
    end subroutine test_nor_rvs_cdp





    subroutine test_nor_pdf_rsp

        real(sp) :: x1, x2(3,4), loc, scale
        integer, parameter :: k = 5
        integer :: seed, get
        real(sp) :: res(3,5)
        real(sp), parameter :: ans(15) =                                   &
                         [0.215050766989949083210785218076278553_sp,       &
                          0.215050766989949083210785218076278553_sp,       &
                          0.215050766989949083210785218076278553_sp,       &
                          0.200537626692880596839299818454439236_sp,       &
                          5.66161527403268434368575024104022496E-0002_sp,  &
                          0.238986957612021514867582359518579138_sp,       &
                          0.265935969411942911029638292783132425_sp,       &
                          0.262147558654079961109031890374902943_sp,       &
                          0.249866408914952245533320687656894701_sp,       &
                          3.98721117498705317877792757313696510E-0002_sp,  &
                          0.265902369803533466897906694845094995_sp,       &
                          0.161311603170650092038944290133124635_sp,       &
                          0.249177740354276111998717092437037695_sp,       &
                          0.237427217242213206474603807278971527_sp,       &
                          0.155696086384122017518186260628090478_sp]

        print *, "Test normal_distribution_pdf_rsp"
        seed = 741852963
        call random_seed(seed, get)
        loc = -0.5_sp; scale = 1.5_sp

        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol),            &
            msg="normal_distribution_pdf_rsp failed", warn=warn)
    end subroutine test_nor_pdf_rsp

    subroutine test_nor_pdf_rdp

        real(dp) :: x1, x2(3,4), loc, scale
        integer, parameter :: k = 5
        integer :: seed, get
        real(dp) :: res(3,5)
        real(dp), parameter :: ans(15) =                                   &
                         [0.215050766989949083210785218076278553_dp,       &
                          0.215050766989949083210785218076278553_dp,       &
                          0.215050766989949083210785218076278553_dp,       &
                          0.200537626692880596839299818454439236_dp,       &
                          5.66161527403268434368575024104022496E-0002_dp,  &
                          0.238986957612021514867582359518579138_dp,       &
                          0.265935969411942911029638292783132425_dp,       &
                          0.262147558654079961109031890374902943_dp,       &
                          0.249866408914952245533320687656894701_dp,       &
                          3.98721117498705317877792757313696510E-0002_dp,  &
                          0.265902369803533466897906694845094995_dp,       &
                          0.161311603170650092038944290133124635_dp,       &
                          0.249177740354276111998717092437037695_dp,       &
                          0.237427217242213206474603807278971527_dp,       &
                          0.155696086384122017518186260628090478_dp]

        print *, "Test normal_distribution_pdf_rdp"
        seed = 741852963
        call random_seed(seed, get)
        loc = -0.5_dp; scale = 1.5_dp

        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol),            &
            msg="normal_distribution_pdf_rdp failed", warn=warn)
    end subroutine test_nor_pdf_rdp

    subroutine test_nor_pdf_csp

        complex(sp) :: x1, x2(3,4), loc, scale
        integer, parameter :: k = 5
        integer :: seed, get
        real(sp) :: res(3,5)
        real(sp), parameter :: ans(15) =                                   &
                         [0.129377311291944176372137325120411497_sp,       &
                          0.129377311291944176372137325120411497_sp,       &
                          0.129377311291944176372137325120411497_sp,       &
                          4.05915662853246811934977653001971736E-0002_sp,  &
                          0.209143395418940756076861773161637924_sp,       &
                          2.98881041363874672676853084975547667E-0002_sp,  &
                          0.128679412679649127469385460133445161_sp,       &
                          0.177484732473055532384223611956177231_sp,       &
                          3.82205306942578982084957100753849738E-0002_sp,  &
                          7.09915714309796034515515428785324918E-0002_sp,  &
                          4.56126582912124629544443072483362352E-0002_sp,  &
                          6.57454133967021123696499056531595921E-0002_sp,  &
                          0.165161039915667041643464172210282279_sp,       &
                          3.86104822953520989775015755966798359E-0002_sp,  &
                          0.196922947431391188040943672442575686_sp]

        print *, "Test normal_distribution_pdf_csp"
        seed = 741852963
        call random_seed(seed, get)
        loc = (-0.5_sp, 0.5_sp); scale = (0.5_sp, 1.5_sp)

        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol),            &
            msg="normal_distribution_pdf_csp failed", warn=warn)
    end subroutine test_nor_pdf_csp

    subroutine test_nor_pdf_cdp

        complex(dp) :: x1, x2(3,4), loc, scale
        integer, parameter :: k = 5
        integer :: seed, get
        real(dp) :: res(3,5)
        real(dp), parameter :: ans(15) =                                   &
                         [0.129377311291944176372137325120411497_dp,       &
                          0.129377311291944176372137325120411497_dp,       &
                          0.129377311291944176372137325120411497_dp,       &
                          4.05915662853246811934977653001971736E-0002_dp,  &
                          0.209143395418940756076861773161637924_dp,       &
                          2.98881041363874672676853084975547667E-0002_dp,  &
                          0.128679412679649127469385460133445161_dp,       &
                          0.177484732473055532384223611956177231_dp,       &
                          3.82205306942578982084957100753849738E-0002_dp,  &
                          7.09915714309796034515515428785324918E-0002_dp,  &
                          4.56126582912124629544443072483362352E-0002_dp,  &
                          6.57454133967021123696499056531595921E-0002_dp,  &
                          0.165161039915667041643464172210282279_dp,       &
                          3.86104822953520989775015755966798359E-0002_dp,  &
                          0.196922947431391188040943672442575686_dp]

        print *, "Test normal_distribution_pdf_cdp"
        seed = 741852963
        call random_seed(seed, get)
        loc = (-0.5_dp, 0.5_dp); scale = (0.5_dp, 1.5_dp)

        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol),            &
            msg="normal_distribution_pdf_cdp failed", warn=warn)
    end subroutine test_nor_pdf_cdp





    subroutine test_nor_cdf_rsp

        real(sp) :: x1, x2(3,4), loc, scale
        integer :: seed, get
        real(sp) :: res(3,5)
        real(sp), parameter :: ans(15) =                                   &
                          [7.50826305038441048487991102776953948E-0002_sp, &
                           7.50826305038441048487991102776953948E-0002_sp, &
                           7.50826305038441048487991102776953948E-0002_sp, &
                           0.143119834108717983250834016885129863_sp,      &
                           0.241425421525703182028420560765471735_sp,      &
                           0.284345878626039240974266199229875972_sp,      &
                           0.233239836366015928845367994433532757_sp,      &
                           0.341059506137219171082517155967522896_sp,      &
                           0.353156850199835111081038166086606192_sp,      &
                           6.81066766396638231790017005897813244E-0002_sp, &
                           4.38792331441682923984716366123285346E-0002_sp, &
                           0.763679637882860826030745070304416929_sp,      &
                           0.363722187587355040667876190724308059_sp,      &
                           0.868187114884980488672309198087692444_sp,      &
                           0.626506799809652872401992867475200722_sp]

        print *, "Test normal_distribution_cdf_rsp"
        seed = 369147582
        call random_seed(seed, get)
        loc = -1.0_sp; scale = 2.0_sp

        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol),             &
            msg="normal_distribution_cdf_rsp failed", warn=warn)
    end subroutine test_nor_cdf_rsp

    subroutine test_nor_cdf_rdp

        real(dp) :: x1, x2(3,4), loc, scale
        integer :: seed, get
        real(dp) :: res(3,5)
        real(dp), parameter :: ans(15) =                                   &
                          [7.50826305038441048487991102776953948E-0002_dp, &
                           7.50826305038441048487991102776953948E-0002_dp, &
                           7.50826305038441048487991102776953948E-0002_dp, &
                           0.143119834108717983250834016885129863_dp,      &
                           0.241425421525703182028420560765471735_dp,      &
                           0.284345878626039240974266199229875972_dp,      &
                           0.233239836366015928845367994433532757_dp,      &
                           0.341059506137219171082517155967522896_dp,      &
                           0.353156850199835111081038166086606192_dp,      &
                           6.81066766396638231790017005897813244E-0002_dp, &
                           4.38792331441682923984716366123285346E-0002_dp, &
                           0.763679637882860826030745070304416929_dp,      &
                           0.363722187587355040667876190724308059_dp,      &
                           0.868187114884980488672309198087692444_dp,      &
                           0.626506799809652872401992867475200722_dp]

        print *, "Test normal_distribution_cdf_rdp"
        seed = 369147582
        call random_seed(seed, get)
        loc = -1.0_dp; scale = 2.0_dp

        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol),             &
            msg="normal_distribution_cdf_rdp failed", warn=warn)
    end subroutine test_nor_cdf_rdp

    subroutine test_nor_cdf_csp

        complex(sp) :: x1, x2(3,4), loc, scale
        integer :: seed, get
        real(sp) :: res(3,5)
        real(sp), parameter :: ans(15) =                                   &
                          [1.07458136221563368133842063954746170E-0002_sp, &
                           1.07458136221563368133842063954746170E-0002_sp, &
                           1.07458136221563368133842063954746170E-0002_sp, &
                           6.86483236063879585051085536740820057E-0002_sp, &
                           7.95486634025192048896990048539218724E-0002_sp, &
                           2.40523393996423661445007940057223384E-0002_sp, &
                           3.35096768781160662250307446207445131E-0002_sp, &
                           0.315778916661119434962814841317323376_sp,      &
                           0.446311293878359175362094845206410428_sp,      &
                           0.102010220821382542292905161748120877_sp,      &
                           7.66919007012121545175655727052974512E-0002_sp, &
                           0.564690968410069125818268877247699603_sp,      &
                           0.708769523556518785240723539383512333_sp,      &
                           6.40553790808161720088070925562830659E-0002_sp, &
                           5.39999153072107729358158443133850711E-0002_sp]

        print *, "Test normal_distribution_cdf_csp"
        seed = 369147582
        call random_seed(seed, get)
        loc = (-1.0_sp, 1.0_sp); scale = (1.7_sp, 2.4_sp)

        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol),             &
            msg="normal_distribution_cdf_csp failed", warn=warn)
    end subroutine test_nor_cdf_csp

    subroutine test_nor_cdf_cdp

        complex(dp) :: x1, x2(3,4), loc, scale
        integer :: seed, get
        real(dp) :: res(3,5)
        real(dp), parameter :: ans(15) =                                   &
                          [1.07458136221563368133842063954746170E-0002_dp, &
                           1.07458136221563368133842063954746170E-0002_dp, &
                           1.07458136221563368133842063954746170E-0002_dp, &
                           6.86483236063879585051085536740820057E-0002_dp, &
                           7.95486634025192048896990048539218724E-0002_dp, &
                           2.40523393996423661445007940057223384E-0002_dp, &
                           3.35096768781160662250307446207445131E-0002_dp, &
                           0.315778916661119434962814841317323376_dp,      &
                           0.446311293878359175362094845206410428_dp,      &
                           0.102010220821382542292905161748120877_dp,      &
                           7.66919007012121545175655727052974512E-0002_dp, &
                           0.564690968410069125818268877247699603_dp,      &
                           0.708769523556518785240723539383512333_dp,      &
                           6.40553790808161720088070925562830659E-0002_dp, &
                           5.39999153072107729358158443133850711E-0002_dp]

        print *, "Test normal_distribution_cdf_cdp"
        seed = 369147582
        call random_seed(seed, get)
        loc = (-1.0_dp, 1.0_dp); scale = (1.7_dp, 2.4_dp)

        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol),             &
            msg="normal_distribution_cdf_cdp failed", warn=warn)
    end subroutine test_nor_cdf_cdp


end program test_distribution_normal
