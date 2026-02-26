program test_distribution_gamma
    use stdlib_kinds, only : sp, dp, xdp
    use stdlib_error, only : check
    use stdlib_random, only : random_seed
    use stdlib_stats_distribution_gamma, only : rgamma => rvs_gamma,           &
                              gamma_pdf => pdf_gamma, gamma_cdf => cdf_gamma

    implicit none
    real(sp), parameter :: sptol = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1.0_dp)
    logical ::  warn = .true.
    integer :: put, get

    put = 12345678
    call random_seed(put, get)

    call test_gamma_random_generator

    call test_gamma_rvs_rsp
    call test_gamma_rvs_rdp
    call test_gamma_rvs_csp
    call test_gamma_rvs_cdp

    call test_gamma_pdf_rsp
    call test_gamma_pdf_rdp
    call test_gamma_pdf_csp
    call test_gamma_pdf_cdp

    call test_gamma_cdf_rsp
    call test_gamma_cdf_rdp
    call test_gamma_cdf_csp
    call test_gamma_cdf_cdp

contains

    subroutine test_gamma_random_generator
        integer, parameter :: num = 10000000, array_size = 1000
        integer :: i, j, freq(0:array_size-1)
        real(dp) :: chisq, expct

        print *, ""
        print *, "Test gamma random generator with chi-squared"

        freq = 0
        do i = 1, num
            j = min(array_size - 1, int(array_size * gamma_cdf(rgamma(2.0_dp, 1.5_dp, 0.0_dp), 2.0_dp, 1.5_dp, 0.0_dp)))
            freq(j) = freq(j) + 1
        end do

        chisq = 0.0_dp
        expct = num / array_size

        do i = 0, array_size - 1
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do

        write(*,*) "The critical values for chi-squared with 1000 d. of f. is" &
            //" 1143.92"
        write(*,*) "Chi-squared for gamma random generator is : ", chisq
        call check((chisq < 1143.9), &
            msg="gamma randomness failed chi-squared test", warn=warn)

    end subroutine test_gamma_random_generator

    subroutine test_gamma_rvs_rsp
        integer, parameter :: k = 5, n = 10
        real(sp) :: res(n), gshape, rate, loc
        integer :: i
        integer :: seed, get
        real(sp) :: ans(n) = [0.85758907497718881E+00_sp,                   &
                0.10206623865526088E+01_sp,                   &
                0.99753931024198652E+00_sp,                   &
                0.97653359790345837E+00_sp,                   &
                0.41853482638322031E+00_sp,                   &
                0.22012288073086305E+01_sp,                   &
                0.20639542613306583E+01_sp,                   &
                0.31794669730880196E+01_sp,                   &
                0.19329744662223283E+01_sp,                   &
                0.10257959670932112E+01_sp]

        print *, "Test gamma_distribution_rvs_rsp"
        seed = 639741825
        call random_seed(seed, get)

        gshape = 2.0_sp; rate = 1.0_sp; loc = 0._sp

        do i = 1, k
            res(i) = rgamma(gshape, rate, loc)
        end do

        res(k + 1 : n) = rgamma(gshape, rate, k, loc)

        do i = 1, n
            call check(abs(res(i) - ans(i)) < sptol,                      &
                       msg="gamma_distribution_rvs_rsp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_rvs_rsp

    subroutine test_gamma_rvs_rdp
        integer, parameter :: k = 5, n = 10
        real(dp) :: res(n), gshape, rate, loc
        integer :: i
        integer :: seed, get
        real(dp) :: ans(n) = [0.85758907497718881E+00_dp,                   &
                0.10206623865526088E+01_dp,                   &
                0.99753931024198652E+00_dp,                   &
                0.97653359790345837E+00_dp,                   &
                0.41853482638322031E+00_dp,                   &
                0.22012288073086305E+01_dp,                   &
                0.20639542613306583E+01_dp,                   &
                0.31794669730880196E+01_dp,                   &
                0.19329744662223283E+01_dp,                   &
                0.10257959670932112E+01_dp]

        print *, "Test gamma_distribution_rvs_rdp"
        seed = 639741825
        call random_seed(seed, get)

        gshape = 2.0_dp; rate = 1.0_dp; loc = 0._dp

        do i = 1, k
            res(i) = rgamma(gshape, rate, loc)
        end do

        res(k + 1 : n) = rgamma(gshape, rate, k, loc)

        do i = 1, n
            call check(abs(res(i) - ans(i)) < dptol,                      &
                       msg="gamma_distribution_rvs_rdp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_rvs_rdp

    subroutine test_gamma_rvs_csp
        integer, parameter :: k = 5, n = 10
        complex(sp) :: res(n), gshape, rate, loc
        integer :: i
        integer :: seed, get
          complex(sp) :: ans(n) =                                                     &
               [(0.10719863437214860E+01_sp, 0.46775532101393817E+00_sp), &
                (0.42382516926807199E+00_sp, 0.96340496644915242E+00_sp), &
                (0.27515360091357881E+01_sp, 0.14837198853150384E+00_sp), &
                (0.14536367104245527E+01_sp, 0.56852736336951546E+00_sp), &
                (0.34559143458416108E+00_sp, 0.49621768536248831E-01_sp), &
                (0.19657884897696515E+01_sp, 0.31124314799641007E+01_sp), &
                (0.34155160623540454E+01_sp, 0.50494893389401867E-01_sp), &
                (0.94594398345216283E+00_sp, 0.45691588305890623E+00_sp), &
                (0.11493158751025962E+01_sp, 0.12944763723941669E+00_sp), &
                (0.29691469633592286E+01_sp, 0.11617408197125868E+01_sp)]

        print *, "Test gamma_distribution_rvs_csp"
        seed = 639741825
        call random_seed(seed, get)

        gshape = (2.0_sp, 0.7_sp); rate = (0.8_sp, 1.2_sp); loc = (0._sp, 0._sp)

        do i = 1, k
            res(i) = rgamma(gshape, rate, loc)
        end do

        res(k + 1 : n) = rgamma(gshape, rate, k, loc)

        do i = 1, n
            call check(abs(res(i) - ans(i)) < sptol,                      &
                       msg="gamma_distribution_rvs_csp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_rvs_csp

    subroutine test_gamma_rvs_cdp
        integer, parameter :: k = 5, n = 10
        complex(dp) :: res(n), gshape, rate, loc
        integer :: i
        integer :: seed, get
          complex(dp) :: ans(n) =                                                     &
               [(0.10719863437214860E+01_dp, 0.46775532101393817E+00_dp), &
                (0.42382516926807199E+00_dp, 0.96340496644915242E+00_dp), &
                (0.27515360091357881E+01_dp, 0.14837198853150384E+00_dp), &
                (0.14536367104245527E+01_dp, 0.56852736336951546E+00_dp), &
                (0.34559143458416108E+00_dp, 0.49621768536248831E-01_dp), &
                (0.19657884897696515E+01_dp, 0.31124314799641007E+01_dp), &
                (0.34155160623540454E+01_dp, 0.50494893389401867E-01_dp), &
                (0.94594398345216283E+00_dp, 0.45691588305890623E+00_dp), &
                (0.11493158751025962E+01_dp, 0.12944763723941669E+00_dp), &
                (0.29691469633592286E+01_dp, 0.11617408197125868E+01_dp)]

        print *, "Test gamma_distribution_rvs_cdp"
        seed = 639741825
        call random_seed(seed, get)

        gshape = (2.0_dp, 0.7_dp); rate = (0.8_dp, 1.2_dp); loc = (0._dp, 0._dp)

        do i = 1, k
            res(i) = rgamma(gshape, rate, loc)
        end do

        res(k + 1 : n) = rgamma(gshape, rate, k, loc)

        do i = 1, n
            call check(abs(res(i) - ans(i)) < dptol,                      &
                       msg="gamma_distribution_rvs_cdp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_rvs_cdp


    subroutine test_gamma_pdf_rsp
        real(sp) :: x1, x2(3,4), gshape, rate, loc
        integer :: i
        integer :: seed, get
        real(sp) :: res(15)
        real(sp), parameter :: ans(15) =                                   &
                 [0.34495412572168702E-01_sp,            &
                  0.34495412572168702E-01_sp,            &
                  0.34495412572168702E-01_sp,            &
                  0.29116634347089576E+00_sp,            &
                  0.28338290850731419E+00_sp,            &
                  0.27922270935613580E+00_sp,            &
                  0.36440665523348268E+00_sp,            &
                  0.24379209619143690E+00_sp,            &
                  0.63815638087140761E-01_sp,            &
                  0.25844600948718583E+00_sp,            &
                  0.17268118913523492E+00_sp,            &
                  0.31181223194308201E+00_sp,            &
                  0.24027095040543100E+00_sp,            &
                  0.36765502365831573E+00_sp,            &
                  0.99011714088769636E-01_sp]

        print *, "Test gamma_distribution_pdf_rsp"
        seed = 345987126
        call random_seed(seed, get)
        gshape = 2.0_sp; rate = 1.0_sp; loc = 0._sp

        x1 = rgamma(gshape, rate, loc)
        x2 = reshape(rgamma(gshape, rate, 12, loc), [3,4])

        res(1:3) = gamma_pdf(x1, gshape, rate, loc)
        res(4:15) = reshape(gamma_pdf(x2, gshape, rate, loc), [12])

        do i = 1, 15
            call check(abs(res(i) - ans(i)) < sptol,                      &
                       msg="gamma_distribution_pdf_rsp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_pdf_rsp

    subroutine test_gamma_pdf_rdp
        real(dp) :: x1, x2(3,4), gshape, rate, loc
        integer :: i
        integer :: seed, get
        real(dp) :: res(15)
        real(dp), parameter :: ans(15) =                                   &
                 [0.34495412572168702E-01_dp,            &
                  0.34495412572168702E-01_dp,            &
                  0.34495412572168702E-01_dp,            &
                  0.29116634347089576E+00_dp,            &
                  0.28338290850731419E+00_dp,            &
                  0.27922270935613580E+00_dp,            &
                  0.36440665523348268E+00_dp,            &
                  0.24379209619143690E+00_dp,            &
                  0.63815638087140761E-01_dp,            &
                  0.25844600948718583E+00_dp,            &
                  0.17268118913523492E+00_dp,            &
                  0.31181223194308201E+00_dp,            &
                  0.24027095040543100E+00_dp,            &
                  0.36765502365831573E+00_dp,            &
                  0.99011714088769636E-01_dp]

        print *, "Test gamma_distribution_pdf_rdp"
        seed = 345987126
        call random_seed(seed, get)
        gshape = 2.0_dp; rate = 1.0_dp; loc = 0._dp

        x1 = rgamma(gshape, rate, loc)
        x2 = reshape(rgamma(gshape, rate, 12, loc), [3,4])

        res(1:3) = gamma_pdf(x1, gshape, rate, loc)
        res(4:15) = reshape(gamma_pdf(x2, gshape, rate, loc), [12])

        do i = 1, 15
            call check(abs(res(i) - ans(i)) < dptol,                      &
                       msg="gamma_distribution_pdf_rdp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_pdf_rdp

    subroutine test_gamma_pdf_csp
        complex(sp) :: x1, x2(3,4), gshape, rate, loc
        integer :: i
        integer :: seed, get
        real(sp) :: res(15)
        real(sp), parameter :: ans(15) =                                   &
                 [0.11554282574059280E+00_sp,            &
                  0.11554282574059280E+00_sp,            &
                  0.11554282574059280E+00_sp,            &
                  0.92682318951901516E-01_sp,            &
                  0.40166849087286064E+00_sp,            &
                  0.37468980496232690E+00_sp,            &
                  0.14712363446345353E+00_sp,            &
                  0.22561628567985192E+00_sp,            &
                  0.12765403024301183E+00_sp,            &
                  0.39182498867847366E-01_sp,            &
                  0.25873533461032836E-02_sp,            &
                  0.10105832622792976E+00_sp,            &
                  0.24044091896609491E+00_sp,            &
                  0.49885356046115958E-02_sp,            &
                  0.11085827028639165E+00_sp]

        print *, "Test gamma_distribution_pdf_csp"
        seed = 345987126
        call random_seed(seed, get)
        gshape = (2.0_sp, 0.7_sp); rate = (0.8_sp, 1.2_sp); loc = (0._sp, 0._sp)

        x1 = rgamma(gshape, rate, loc)
        x2 = reshape(rgamma(gshape, rate, 12, loc), [3,4])

        res(1:3) = gamma_pdf(x1, gshape, rate, loc)
        res(4:15) = reshape(gamma_pdf(x2, gshape, rate, loc), [12])

        do i = 1, 15
            call check(abs(res(i) - ans(i)) < sptol,                      &
                       msg="gamma_distribution_pdf_csp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_pdf_csp

    subroutine test_gamma_pdf_cdp
        complex(dp) :: x1, x2(3,4), gshape, rate, loc
        integer :: i
        integer :: seed, get
        real(dp) :: res(15)
        real(dp), parameter :: ans(15) =                                   &
                 [0.11554282574059280E+00_dp,            &
                  0.11554282574059280E+00_dp,            &
                  0.11554282574059280E+00_dp,            &
                  0.92682318951901516E-01_dp,            &
                  0.40166849087286064E+00_dp,            &
                  0.37468980496232690E+00_dp,            &
                  0.14712363446345353E+00_dp,            &
                  0.22561628567985192E+00_dp,            &
                  0.12765403024301183E+00_dp,            &
                  0.39182498867847366E-01_dp,            &
                  0.25873533461032836E-02_dp,            &
                  0.10105832622792976E+00_dp,            &
                  0.24044091896609491E+00_dp,            &
                  0.49885356046115958E-02_dp,            &
                  0.11085827028639165E+00_dp]

        print *, "Test gamma_distribution_pdf_cdp"
        seed = 345987126
        call random_seed(seed, get)
        gshape = (2.0_dp, 0.7_dp); rate = (0.8_dp, 1.2_dp); loc = (0._dp, 0._dp)

        x1 = rgamma(gshape, rate, loc)
        x2 = reshape(rgamma(gshape, rate, 12, loc), [3,4])

        res(1:3) = gamma_pdf(x1, gshape, rate, loc)
        res(4:15) = reshape(gamma_pdf(x2, gshape, rate, loc), [12])

        do i = 1, 15
            call check(abs(res(i) - ans(i)) < dptol,                      &
                       msg="gamma_distribution_pdf_cdp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_pdf_cdp


    subroutine test_gamma_cdf_rsp
        real(sp) :: x1, x2(3,4), gshape, rate, loc
        integer :: i
        integer :: seed, get
        real(sp) :: res(15)
        real(sp), parameter :: ans(15) =                                   &
                 [0.95856447024923161E+00_sp,            &
                  0.95856447024923161E+00_sp,            &
                  0.95856447024923161E+00_sp,            &
                  0.78989843937516711E-01_sp,            &
                  0.56793164492527640E+00_sp,            &
                  0.69398686246667538E-01_sp,            &
                  0.31709086664429768E+00_sp,            &
                  0.64538378729933876E+00_sp,            &
                  0.92092738488690244E+00_sp,            &
                  0.61792198504673790E+00_sp,            &
                  0.19854195697723341E-01_sp,            &
                  0.99267229424260651E-01_sp,            &
                  0.65180771852756031E+00_sp,            &
                  0.25154115213140924E+00_sp,            &
                  0.56823696777839551E-02_sp]

        print *, "Test gamma_distribution_cdf_rsp"
        seed = 345987126
        call random_seed(seed, get)
        gshape = 2.0_sp; rate = 1.0_sp; loc = 0._sp

        x1 = rgamma(gshape, rate, loc)
        x2 = reshape(rgamma(gshape, rate, 12, loc), [3,4])

        res(1:3) = gamma_cdf(x1, gshape, rate, loc)
        res(4:15) = reshape(gamma_cdf(x2, gshape, rate, loc), [12])

        do i = 1, 15
            call check(abs(res(i) - ans(i)) < sptol,                      &
                       msg="gamma_distribution_cdf_rsp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_cdf_rsp

    subroutine test_gamma_cdf_rdp
        real(dp) :: x1, x2(3,4), gshape, rate, loc
        integer :: i
        integer :: seed, get
        real(dp) :: res(15)
        real(dp), parameter :: ans(15) =                                   &
                 [0.95856447024923161E+00_dp,            &
                  0.95856447024923161E+00_dp,            &
                  0.95856447024923161E+00_dp,            &
                  0.78989843937516711E-01_dp,            &
                  0.56793164492527640E+00_dp,            &
                  0.69398686246667538E-01_dp,            &
                  0.31709086664429768E+00_dp,            &
                  0.64538378729933876E+00_dp,            &
                  0.92092738488690244E+00_dp,            &
                  0.61792198504673790E+00_dp,            &
                  0.19854195697723341E-01_dp,            &
                  0.99267229424260651E-01_dp,            &
                  0.65180771852756031E+00_dp,            &
                  0.25154115213140924E+00_dp,            &
                  0.56823696777839551E-02_dp]

        print *, "Test gamma_distribution_cdf_rdp"
        seed = 345987126
        call random_seed(seed, get)
        gshape = 2.0_dp; rate = 1.0_dp; loc = 0._dp

        x1 = rgamma(gshape, rate, loc)
        x2 = reshape(rgamma(gshape, rate, 12, loc), [3,4])

        res(1:3) = gamma_cdf(x1, gshape, rate, loc)
        res(4:15) = reshape(gamma_cdf(x2, gshape, rate, loc), [12])

        do i = 1, 15
            call check(abs(res(i) - ans(i)) < dptol,                      &
                       msg="gamma_distribution_cdf_rdp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_cdf_rdp

    subroutine test_gamma_cdf_csp
        complex(sp) :: x1, x2(3,4), gshape, rate, loc
        integer :: i
        integer :: seed, get
        real(sp) :: res(15)
        real(sp), parameter :: ans(15) =                                   &
                 [0.30546556548076713E-01_sp,            &
                  0.30546556548076713E-01_sp,            &
                  0.30546556548076713E-01_sp,            &
                  0.28163392764814024E+00_sp,            &
                  0.93663475343392596E-01_sp,            &
                  0.34623100575641867E-02_sp,            &
                  0.33855418983076070E+00_sp,            &
                  0.65637763837417527E-01_sp,            &
                  0.15878146335804994E+00_sp,            &
                  0.48425923323146416E+00_sp,            &
                  0.85584614493283579E+00_sp,            &
                  0.41677260318284231E+00_sp,            &
                  0.33377215439459634E-01_sp,            &
                  0.48072346126598581E+00_sp,            &
                  0.38686165822935510E+00_sp]

        print *, "Test gamma_distribution_cdf_csp"
        seed = 345987126
        call random_seed(seed, get)
        gshape = (2.0_sp, 0.7_sp); rate = (0.8_sp, 1.2_sp); loc = (0._sp, 0._sp)

        x1 = rgamma(gshape, rate, loc)
        x2 = reshape(rgamma(gshape, rate, 12, loc), [3,4])

        res(1:3) = gamma_cdf(x1, gshape, rate, loc)
        res(4:15) = reshape(gamma_cdf(x2, gshape, rate, loc), [12])

        do i = 1, 15
            call check(abs(res(i) - ans(i)) < sptol,                      &
                       msg="gamma_distribution_cdf_csp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_cdf_csp

    subroutine test_gamma_cdf_cdp
        complex(dp) :: x1, x2(3,4), gshape, rate, loc
        integer :: i
        integer :: seed, get
        real(dp) :: res(15)
        real(dp), parameter :: ans(15) =                                   &
                 [0.30546556548076713E-01_dp,            &
                  0.30546556548076713E-01_dp,            &
                  0.30546556548076713E-01_dp,            &
                  0.28163392764814024E+00_dp,            &
                  0.93663475343392596E-01_dp,            &
                  0.34623100575641867E-02_dp,            &
                  0.33855418983076070E+00_dp,            &
                  0.65637763837417527E-01_dp,            &
                  0.15878146335804994E+00_dp,            &
                  0.48425923323146416E+00_dp,            &
                  0.85584614493283579E+00_dp,            &
                  0.41677260318284231E+00_dp,            &
                  0.33377215439459634E-01_dp,            &
                  0.48072346126598581E+00_dp,            &
                  0.38686165822935510E+00_dp]

        print *, "Test gamma_distribution_cdf_cdp"
        seed = 345987126
        call random_seed(seed, get)
        gshape = (2.0_dp, 0.7_dp); rate = (0.8_dp, 1.2_dp); loc = (0._dp, 0._dp)

        x1 = rgamma(gshape, rate, loc)
        x2 = reshape(rgamma(gshape, rate, 12, loc), [3,4])

        res(1:3) = gamma_cdf(x1, gshape, rate, loc)
        res(4:15) = reshape(gamma_cdf(x2, gshape, rate, loc), [12])

        do i = 1, 15
            call check(abs(res(i) - ans(i)) < dptol,                      &
                       msg="gamma_distribution_cdf_cdp failed", &
                       warn=warn)
        end do
    end subroutine test_gamma_cdf_cdp


end program test_distribution_gamma
