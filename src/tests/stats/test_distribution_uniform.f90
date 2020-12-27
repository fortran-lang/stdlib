program test_distribution_uniform
    use stdlib_error, only : check
    use stdlib_kinds
    use stdlib_stats_distribution_PRNG, only : random_seed, dist_rand
    use stdlib_stats_distribution_uniform,                                      &
        uni_rvs => uniform_distribution_rvs,                                    &
        uni_pdf => uniform_distribution_pdf,                                    &
        uni_cdf => uniform_distribution_cdf

    implicit none
    logical ::  warn = .true.
    real(sp), parameter :: sptol = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1.0_dp)
    real(qp), parameter :: qptol = 1000 * epsilon(1.0_qp)
    integer :: put, get

    put = 135792468

    call test_shuffle

    call test_uni_rvs_0

    call test_uni_rvs_iint8
    call test_uni_rvs_iint16
    call test_uni_rvs_iint32
    call test_uni_rvs_iint64
    call test_uni_rvs_rsp
    call test_uni_rvs_rdp
    call test_uni_rvs_rqp
    call test_uni_rvs_csp
    call test_uni_rvs_cdp
    call test_uni_rvs_cqp

    call test_uni_pdf_iint8
    call test_uni_pdf_iint16
    call test_uni_pdf_iint32
    call test_uni_pdf_iint64
    call test_uni_pdf_rsp
    call test_uni_pdf_rdp
    call test_uni_pdf_rqp
    call test_uni_pdf_csp
    call test_uni_pdf_cdp
    call test_uni_pdf_cqp

    call test_uni_cdf_iint8
    call test_uni_cdf_iint16
    call test_uni_cdf_iint32
    call test_uni_cdf_iint64
    call test_uni_cdf_rsp
    call test_uni_cdf_rdp
    call test_uni_cdf_rqp
    call test_uni_cdf_csp
    call test_uni_cdf_cdp
    call test_uni_cdf_cqp
    stop

    contains

    subroutine test_shuffle
        integer :: n(10), na(10) = [10, 6, 9, 2, 8, 1, 3, 5, 7, 4]
        real :: x(10),  xa(10)= [5.0, 10.0, 9.0, 4.0, 3.0, 8.0, 2.0, 1.0,       &
                                 7.0, 6.0]
        complex :: z(10), za(10)=[(8.0, 8.0), (7.0, 7.0), (4.0, 4.0),           &
                                  (1.0, 1.0), (5.0, 5.0), (9.0, 9.0),           &
                                  (6.0, 6.0), (3.0, 3.0), (2.0, 2.0),           &
                                  (10.0, 10.0)]
        integer :: i, put, get

        do i=1, 10
            n(i) = i
            x(i) = real(i)
            z(i) = cmplx(real(i),real(i))
        end do
        put = 32165498
        call random_seed(put, get)
        n(:) = shuffle(n)
        x(:) = shuffle(x)
        z(:) = shuffle(z)
        call check(all(n == na), &
             msg="Integer shuffle failed test", warn=warn)
        call check(all(x == xa), &
             msg="Real shuffle failed test", warn=warn)
        call check(all(z == za), &
             msg="Complex shuffle failed test", warn=warn)
    end subroutine test_shuffle

    subroutine test_uni_rvs_0
        integer :: i, j, freq(0:1000), num=10000000
        real(dp) :: chisq, expct

        print *,""
        print *, "Test uniform random generator with chi-squared"
        freq = 0
        do i = 1, num
            j = 1000 * uni_rvs( )
            freq(j) = freq(j) + 1
        end do
        chisq = 0.0_dp
        expct = num / 1000
        do i = 0, 999
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do
        write(*,*) "The critical values for chi-squared with 1000 d. of f. are" &
                   //" 1143.92"
        write(*,*) "Chi-squared for uniform random generator is : ", chisq
        call check((chisq < 1143.9) , &
               msg="uniform randomness failed chi-squared test", warn=warn)
    end subroutine test_uni_rvs_0

      subroutine test_uni_rvs_iint8
          integer(int8) :: res(15), scale, loc
          integer :: i, n, seed, get, k

          integer(int8) :: ans(15) = [47, 99, 43, 37, 48, 30, 27, 100, 30, 33, 21,     &
                               103, 55, 54, 110]

          print *, "Test uniform_distribution_rvs_iint8"
          seed = 258147369; k = 5
          call random_seed(seed, get)
          loc = 15_int8; scale = 100_int8
          do i=1, 5
              res(i) = uni_rvs(scale)             ! 1 dummy
          end do
          do i=6,10
              res(i) = uni_rvs(loc, scale)        ! 2 dummies
          end do
          res(11:15) = uni_rvs(loc, scale, k)     ! 3 dummies
          call check(all(res == ans), &
              msg="uniform_distribution_rvs_iint8 failed", warn=warn)
      end subroutine test_uni_rvs_iint8

      subroutine test_uni_rvs_iint16
          integer(int16) :: res(15), scale, loc
          integer :: i, n, seed, get, k

          integer(int16) :: ans(15) = [25, 4, 81, 98, 49, 34, 32, 62, 115, 112, 26,     &
                               20, 37, 100, 82]

          print *, "Test uniform_distribution_rvs_iint16"
          seed = 258147369; k = 5
          call random_seed(seed, get)
          loc = 15_int16; scale = 100_int16
          do i=1, 5
              res(i) = uni_rvs(scale)             ! 1 dummy
          end do
          do i=6,10
              res(i) = uni_rvs(loc, scale)        ! 2 dummies
          end do
          res(11:15) = uni_rvs(loc, scale, k)     ! 3 dummies
          call check(all(res == ans), &
              msg="uniform_distribution_rvs_iint16 failed", warn=warn)
      end subroutine test_uni_rvs_iint16

      subroutine test_uni_rvs_iint32
          integer(int32) :: res(15), scale, loc
          integer :: i, n, seed, get, k

          integer(int32) :: ans(15) = [19, 52, 56, 20, 59, 44, 34, 102, 19, 39, 60,     &
                               50, 97, 56, 67]

          print *, "Test uniform_distribution_rvs_iint32"
          seed = 258147369; k = 5
          call random_seed(seed, get)
          loc = 15_int32; scale = 100_int32
          do i=1, 5
              res(i) = uni_rvs(scale)             ! 1 dummy
          end do
          do i=6,10
              res(i) = uni_rvs(loc, scale)        ! 2 dummies
          end do
          res(11:15) = uni_rvs(loc, scale, k)     ! 3 dummies
          call check(all(res == ans), &
              msg="uniform_distribution_rvs_iint32 failed", warn=warn)
      end subroutine test_uni_rvs_iint32

      subroutine test_uni_rvs_iint64
          integer(int64) :: res(15), scale, loc
          integer :: i, n, seed, get, k

          integer(int64) :: ans(15) = [76, 45, 43, 75, 76, 15, 25, 24, 114, 113, 94,    &
                               29, 109, 93, 89]

          print *, "Test uniform_distribution_rvs_iint64"
          seed = 258147369; k = 5
          call random_seed(seed, get)
          loc = 15_int64; scale = 100_int64
          do i=1, 5
              res(i) = uni_rvs(scale)             ! 1 dummy
          end do
          do i=6,10
              res(i) = uni_rvs(loc, scale)        ! 2 dummies
          end do
          res(11:15) = uni_rvs(loc, scale, k)     ! 3 dummies
          call check(all(res == ans), &
              msg="uniform_distribution_rvs_iint64 failed", warn=warn)
      end subroutine test_uni_rvs_iint64

      subroutine test_uni_rvs_rsp
          real(sp) :: res(15), scale, loc
          integer :: i, n, seed, get, k

          real(sp) :: ans(15) =                                                   &
                          [0.914826628538749186958511927514337003_sp,       &
                          0.367330098664966409049981166390352882_sp,        &
                          1.77591243057709280428468900936422870_sp,         &
                          0.885921308987590139238932351872790605_sp,        &
                          0.950735656542987861428173346212133765_sp,        &
                          -0.659562573857055134407545438079978339_sp,       &
                          -0.116661718506947176265953203255776316_sp,       &
                          0.837391893893859151631886561517603695_sp,        &
                          -0.703954396598600540269075054311542772_sp,       &
                          0.382592729851141566399519433616660535_sp,        &
                          -0.132472493978185168472805344208609313_sp,       &
                          -0.878723366294216184924081858298450243_sp,       &
                          -0.901660046141515819639877804547722917_sp,       &
                          -0.164090614147737401395943379611708224_sp,       &
                          -0.333886718190384290672056977200554684_sp]

          print *, "Test uniform_distribution_rvs_rsp"
          seed = 258147369; k = 5
          call random_seed(seed, get)
          loc = -1.0_sp; scale = 2.0_sp
          do i=1, 5
              res(i) = uni_rvs(scale)             ! 1 dummy
          end do
          do i=6,10
              res(i) = uni_rvs(loc, scale)        ! 2 dummies
          end do
          res(11:15) = uni_rvs(loc, scale, k)     ! 3 dummies
          call check(all(abs(res - ans) < sptol),                           &
            msg="uniform_distribution_rvs_rsp failed", warn=warn)
      end subroutine test_uni_rvs_rsp

      subroutine test_uni_rvs_rdp
          real(dp) :: res(15), scale, loc
          integer :: i, n, seed, get, k

          real(dp) :: ans(15) =                                                   &
                          [0.914826628538749186958511927514337003_dp,       &
                          0.367330098664966409049981166390352882_dp,        &
                          1.77591243057709280428468900936422870_dp,         &
                          0.885921308987590139238932351872790605_dp,        &
                          0.950735656542987861428173346212133765_dp,        &
                          -0.659562573857055134407545438079978339_dp,       &
                          -0.116661718506947176265953203255776316_dp,       &
                          0.837391893893859151631886561517603695_dp,        &
                          -0.703954396598600540269075054311542772_dp,       &
                          0.382592729851141566399519433616660535_dp,        &
                          -0.132472493978185168472805344208609313_dp,       &
                          -0.878723366294216184924081858298450243_dp,       &
                          -0.901660046141515819639877804547722917_dp,       &
                          -0.164090614147737401395943379611708224_dp,       &
                          -0.333886718190384290672056977200554684_dp]

          print *, "Test uniform_distribution_rvs_rdp"
          seed = 258147369; k = 5
          call random_seed(seed, get)
          loc = -1.0_dp; scale = 2.0_dp
          do i=1, 5
              res(i) = uni_rvs(scale)             ! 1 dummy
          end do
          do i=6,10
              res(i) = uni_rvs(loc, scale)        ! 2 dummies
          end do
          res(11:15) = uni_rvs(loc, scale, k)     ! 3 dummies
          call check(all(abs(res - ans) < dptol),                           &
            msg="uniform_distribution_rvs_rdp failed", warn=warn)
      end subroutine test_uni_rvs_rdp

      subroutine test_uni_rvs_rqp
          real(qp) :: res(15), scale, loc
          integer :: i, n, seed, get, k

          real(qp) :: ans(15) =                                                   &
                          [0.914826628538749186958511927514337003_qp,       &
                          0.367330098664966409049981166390352882_qp,        &
                          1.77591243057709280428468900936422870_qp,         &
                          0.885921308987590139238932351872790605_qp,        &
                          0.950735656542987861428173346212133765_qp,        &
                          -0.659562573857055134407545438079978339_qp,       &
                          -0.116661718506947176265953203255776316_qp,       &
                          0.837391893893859151631886561517603695_qp,        &
                          -0.703954396598600540269075054311542772_qp,       &
                          0.382592729851141566399519433616660535_qp,        &
                          -0.132472493978185168472805344208609313_qp,       &
                          -0.878723366294216184924081858298450243_qp,       &
                          -0.901660046141515819639877804547722917_qp,       &
                          -0.164090614147737401395943379611708224_qp,       &
                          -0.333886718190384290672056977200554684_qp]

          print *, "Test uniform_distribution_rvs_rqp"
          seed = 258147369; k = 5
          call random_seed(seed, get)
          loc = -1.0_qp; scale = 2.0_qp
          do i=1, 5
              res(i) = uni_rvs(scale)             ! 1 dummy
          end do
          do i=6,10
              res(i) = uni_rvs(loc, scale)        ! 2 dummies
          end do
          res(11:15) = uni_rvs(loc, scale, k)     ! 3 dummies
          call check(all(abs(res - ans) < qptol),                           &
            msg="uniform_distribution_rvs_rqp failed", warn=warn)
      end subroutine test_uni_rvs_rqp

      subroutine test_uni_rvs_csp
          complex(sp) :: res(15), scale, loc
          integer :: i, n, seed, get, k

          complex(sp) :: ans(15) = [(0.457413314269374593479255963757168502_sp,  &
                                0.183665049332483204524990583195176441_sp), &
                               (0.887956215288546402142344504682114348_sp,  &
                                0.442960654493795069619466175936395302_sp), &
                               (0.475367828271493930714086673106066883_sp,  &
                                0.170218713071472432796227280960010830_sp), &
                               (0.441669140746526411867023398372111842_sp,  &
                                0.918695946946929575815943280758801848_sp), &
                               (0.148022801700699729865462472844228614_sp,  &
                                0.691296364925570783199759716808330268_sp), &
                        (-6.623624698909258423640267210430465639E-0002_sp,  &
                                0.560638316852891907537959070850774879_sp), &
                              (-0.450830023070757909819938902273861459_sp,  &
                                0.917954692926131299302028310194145888_sp), &
                              (-0.166943359095192145336028488600277342_sp,  &
                                 1.05997401970850635422038976685144007_sp), &
                              (-0.429652190199228276035192664039641386_sp,  &
                                0.523558274341032421628217008446881664_sp), &
                               (0.427181091476823815433760955784237012_sp,  &
                                 1.34628934976074521312483511792379431_sp), &
                              (-0.343281426018765739582860874179459643_sp,  &
                                 1.15357331316264255516301773241139017_sp), &
                              (-0.127590074749816595467422075671493076_sp,  &
                                 1.06891199479835175001340985545539297_sp), &
                               (0.262287586904722758163188700564205647_sp,  &
                                 1.29508919831907332032017166056903079_sp), &
                              (-0.192677407376582732201342196276527829_sp,  &
                                 1.32794925614337933073016984053538181_sp), &
                              (-0.264742129752461530234342035328154452_sp,  &
                                 1.01282963412172621886497836385387927_sp)]

          print *, "Test uniform_distribution_rvs_csp"
          seed = 258147369; k = 5
          call random_seed(seed, get)
          loc = (-0.5_sp,0.5_sp); scale = (1.0_sp, 1.0_sp)
          do i=1, 5
              res(i) = uni_rvs(scale)             ! 1 dummy
          end do
          do i=6,10
              res(i) = uni_rvs(loc, scale)        ! 2 dummies
          end do
          res(11:15) = uni_rvs(loc, scale, k)     ! 3 dummies
          call check(all(abs(real(res) - real(ans)) < sptol) .and.          &
                     all(abs(aimag(res) - aimag(ans)) < sptol),             &
            msg="uniform_distribution_rvs_csp failed", warn=warn)
      end subroutine test_uni_rvs_csp

      subroutine test_uni_rvs_cdp
          complex(dp) :: res(15), scale, loc
          integer :: i, n, seed, get, k

          complex(dp) :: ans(15) = [(0.457413314269374593479255963757168502_dp,  &
                                0.183665049332483204524990583195176441_dp), &
                               (0.887956215288546402142344504682114348_dp,  &
                                0.442960654493795069619466175936395302_dp), &
                               (0.475367828271493930714086673106066883_dp,  &
                                0.170218713071472432796227280960010830_dp), &
                               (0.441669140746526411867023398372111842_dp,  &
                                0.918695946946929575815943280758801848_dp), &
                               (0.148022801700699729865462472844228614_dp,  &
                                0.691296364925570783199759716808330268_dp), &
                        (-6.623624698909258423640267210430465639E-0002_dp,  &
                                0.560638316852891907537959070850774879_dp), &
                              (-0.450830023070757909819938902273861459_dp,  &
                                0.917954692926131299302028310194145888_dp), &
                              (-0.166943359095192145336028488600277342_dp,  &
                                 1.05997401970850635422038976685144007_dp), &
                              (-0.429652190199228276035192664039641386_dp,  &
                                0.523558274341032421628217008446881664_dp), &
                               (0.427181091476823815433760955784237012_dp,  &
                                 1.34628934976074521312483511792379431_dp), &
                              (-0.343281426018765739582860874179459643_dp,  &
                                 1.15357331316264255516301773241139017_dp), &
                              (-0.127590074749816595467422075671493076_dp,  &
                                 1.06891199479835175001340985545539297_dp), &
                               (0.262287586904722758163188700564205647_dp,  &
                                 1.29508919831907332032017166056903079_dp), &
                              (-0.192677407376582732201342196276527829_dp,  &
                                 1.32794925614337933073016984053538181_dp), &
                              (-0.264742129752461530234342035328154452_dp,  &
                                 1.01282963412172621886497836385387927_dp)]

          print *, "Test uniform_distribution_rvs_cdp"
          seed = 258147369; k = 5
          call random_seed(seed, get)
          loc = (-0.5_dp,0.5_dp); scale = (1.0_dp, 1.0_dp)
          do i=1, 5
              res(i) = uni_rvs(scale)             ! 1 dummy
          end do
          do i=6,10
              res(i) = uni_rvs(loc, scale)        ! 2 dummies
          end do
          res(11:15) = uni_rvs(loc, scale, k)     ! 3 dummies
          call check(all(abs(real(res) - real(ans)) < dptol) .and.          &
                     all(abs(aimag(res) - aimag(ans)) < dptol),             &
            msg="uniform_distribution_rvs_cdp failed", warn=warn)
      end subroutine test_uni_rvs_cdp

      subroutine test_uni_rvs_cqp
          complex(qp) :: res(15), scale, loc
          integer :: i, n, seed, get, k

          complex(qp) :: ans(15) = [(0.457413314269374593479255963757168502_qp,  &
                                0.183665049332483204524990583195176441_qp), &
                               (0.887956215288546402142344504682114348_qp,  &
                                0.442960654493795069619466175936395302_qp), &
                               (0.475367828271493930714086673106066883_qp,  &
                                0.170218713071472432796227280960010830_qp), &
                               (0.441669140746526411867023398372111842_qp,  &
                                0.918695946946929575815943280758801848_qp), &
                               (0.148022801700699729865462472844228614_qp,  &
                                0.691296364925570783199759716808330268_qp), &
                        (-6.623624698909258423640267210430465639E-0002_qp,  &
                                0.560638316852891907537959070850774879_qp), &
                              (-0.450830023070757909819938902273861459_qp,  &
                                0.917954692926131299302028310194145888_qp), &
                              (-0.166943359095192145336028488600277342_qp,  &
                                 1.05997401970850635422038976685144007_qp), &
                              (-0.429652190199228276035192664039641386_qp,  &
                                0.523558274341032421628217008446881664_qp), &
                               (0.427181091476823815433760955784237012_qp,  &
                                 1.34628934976074521312483511792379431_qp), &
                              (-0.343281426018765739582860874179459643_qp,  &
                                 1.15357331316264255516301773241139017_qp), &
                              (-0.127590074749816595467422075671493076_qp,  &
                                 1.06891199479835175001340985545539297_qp), &
                               (0.262287586904722758163188700564205647_qp,  &
                                 1.29508919831907332032017166056903079_qp), &
                              (-0.192677407376582732201342196276527829_qp,  &
                                 1.32794925614337933073016984053538181_qp), &
                              (-0.264742129752461530234342035328154452_qp,  &
                                 1.01282963412172621886497836385387927_qp)]

          print *, "Test uniform_distribution_rvs_cqp"
          seed = 258147369; k = 5
          call random_seed(seed, get)
          loc = (-0.5_qp,0.5_qp); scale = (1.0_qp, 1.0_qp)
          do i=1, 5
              res(i) = uni_rvs(scale)             ! 1 dummy
          end do
          do i=6,10
              res(i) = uni_rvs(loc, scale)        ! 2 dummies
          end do
          res(11:15) = uni_rvs(loc, scale, k)     ! 3 dummies
          call check(all(abs(real(res) - real(ans)) < qptol) .and.          &
                     all(abs(aimag(res) - aimag(ans)) < qptol),             &
            msg="uniform_distribution_rvs_cqp failed", warn=warn)
      end subroutine test_uni_rvs_cqp


    subroutine test_uni_pdf_iint8
        integer(int8) :: x1, x2(3,4), loc, scale
        integer :: seed, get, i
        real :: res(3,5)
        real :: ans(15) = [(1.96078438E-02, i=1,15)]

        print *, "Test uniform_distribution_pdf_iint8"
        seed = 147258639
        call random_seed(seed, get)
        loc = 0_int8; scale = 50_int8
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_pdf_iint8 failed", warn=warn)
    end subroutine test_uni_pdf_iint8

    subroutine test_uni_pdf_iint16
        integer(int16) :: x1, x2(3,4), loc, scale
        integer :: seed, get, i
        real :: res(3,5)
        real :: ans(15) = [(1.96078438E-02, i=1,15)]

        print *, "Test uniform_distribution_pdf_iint16"
        seed = 147258639
        call random_seed(seed, get)
        loc = 0_int16; scale = 50_int16
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_pdf_iint16 failed", warn=warn)
    end subroutine test_uni_pdf_iint16

    subroutine test_uni_pdf_iint32
        integer(int32) :: x1, x2(3,4), loc, scale
        integer :: seed, get, i
        real :: res(3,5)
        real :: ans(15) = [(1.96078438E-02, i=1,15)]

        print *, "Test uniform_distribution_pdf_iint32"
        seed = 147258639
        call random_seed(seed, get)
        loc = 0_int32; scale = 50_int32
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_pdf_iint32 failed", warn=warn)
    end subroutine test_uni_pdf_iint32

    subroutine test_uni_pdf_iint64
        integer(int64) :: x1, x2(3,4), loc, scale
        integer :: seed, get, i
        real :: res(3,5)
        real :: ans(15) = [(1.96078438E-02, i=1,15)]

        print *, "Test uniform_distribution_pdf_iint64"
        seed = 147258639
        call random_seed(seed, get)
        loc = 0_int64; scale = 50_int64
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_pdf_iint64 failed", warn=warn)
    end subroutine test_uni_pdf_iint64

    subroutine test_uni_pdf_rsp
        real(sp) :: x1, x2(3,4), loc, scale
        integer :: seed, get, i
        real :: res(3,5)
        real :: ans(15) = [(0.5, i=1,15)]

        print *, "Test uniform_distribution_pdf_rsp"
        seed = 147258639
        call random_seed(seed, get)
        loc = 0.0_sp; scale = 2.0_sp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_pdf_rsp failed", warn=warn)
    end subroutine test_uni_pdf_rsp

    subroutine test_uni_pdf_rdp
        real(dp) :: x1, x2(3,4), loc, scale
        integer :: seed, get, i
        real :: res(3,5)
        real :: ans(15) = [(0.5, i=1,15)]

        print *, "Test uniform_distribution_pdf_rdp"
        seed = 147258639
        call random_seed(seed, get)
        loc = 0.0_dp; scale = 2.0_dp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_pdf_rdp failed", warn=warn)
    end subroutine test_uni_pdf_rdp

    subroutine test_uni_pdf_rqp
        real(qp) :: x1, x2(3,4), loc, scale
        integer :: seed, get, i
        real :: res(3,5)
        real :: ans(15) = [(0.5, i=1,15)]

        print *, "Test uniform_distribution_pdf_rqp"
        seed = 147258639
        call random_seed(seed, get)
        loc = 0.0_qp; scale = 2.0_qp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_pdf_rqp failed", warn=warn)
    end subroutine test_uni_pdf_rqp

    subroutine test_uni_pdf_csp
        complex(sp) :: x1, x2(3,4), loc, scale
        integer :: seed, get, i
        real :: res(3,5)
        real :: ans(15) = [(1.0, i=1,15)]

        print *, "Test uniform_distribution_pdf_csp"
        seed = 147258639
        call random_seed(seed, get)
        loc = (-0.5_sp, 0.5_sp); scale = (1.0_sp, 1.0_sp)
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_pdf_csp failed", warn=warn)
    end subroutine test_uni_pdf_csp

    subroutine test_uni_pdf_cdp
        complex(dp) :: x1, x2(3,4), loc, scale
        integer :: seed, get, i
        real :: res(3,5)
        real :: ans(15) = [(1.0, i=1,15)]

        print *, "Test uniform_distribution_pdf_cdp"
        seed = 147258639
        call random_seed(seed, get)
        loc = (-0.5_dp, 0.5_dp); scale = (1.0_dp, 1.0_dp)
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_pdf_cdp failed", warn=warn)
    end subroutine test_uni_pdf_cdp

    subroutine test_uni_pdf_cqp
        complex(qp) :: x1, x2(3,4), loc, scale
        integer :: seed, get, i
        real :: res(3,5)
        real :: ans(15) = [(1.0, i=1,15)]

        print *, "Test uniform_distribution_pdf_cqp"
        seed = 147258639
        call random_seed(seed, get)
        loc = (-0.5_qp, 0.5_qp); scale = (1.0_qp, 1.0_qp)
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_pdf_cqp failed", warn=warn)
    end subroutine test_uni_pdf_cqp


    subroutine test_uni_cdf_iint8
        integer(int8) :: x1, x2(3,4), loc, scale
        real :: res(3,5)
        integer :: seed, get
        real :: ans(15) = [0.435643554, 0.435643554, 0.435643554, 0.702970326,  &
                           0.653465331, 0.485148519, 0.386138618, 0.386138618,  &
                           0.336633652, 0.277227730, 0.237623766, 0.524752498,  &
                           0.732673287, 0.534653485, 0.415841579]

        print *, "Test uniform_distribution_cdf_iint8"
        seed = 369147258
        call random_seed(seed, get)
        loc = 14_int8; scale = 100_int8
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_cdf_iint8 failed", warn=warn)
    end subroutine test_uni_cdf_iint8

    subroutine test_uni_cdf_iint16
        integer(int16) :: x1, x2(3,4), loc, scale
        real :: res(3,5)
        integer :: seed, get
        real :: ans(15) = [0.178217828, 0.178217828, 0.178217828, 0.465346545,  &
                           0.673267305, 0.247524753, 0.158415839, 0.792079210,  &
                           0.742574275, 0.574257433, 0.881188095, 0.663366318,  &
                           0.524752498, 0.623762369, 0.178217828]

        print *, "Test uniform_distribution_cdf_iint16"
        seed = 369147258
        call random_seed(seed, get)
        loc = 14_int16; scale = 100_int16
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_cdf_iint16 failed", warn=warn)
    end subroutine test_uni_cdf_iint16

    subroutine test_uni_cdf_iint32
        integer(int32) :: x1, x2(3,4), loc, scale
        real :: res(3,5)
        integer :: seed, get
        real :: ans(15) = [0.732673287, 0.732673287, 0.732673287, 0.722772300,  &
                           0.792079210, 5.94059415E-02, 0.841584146,0.405940592,&
                           0.960396051, 0.534653485, 0.782178223, 0.861386120,  &
                           0.564356446, 0.613861382, 0.306930691]

        print *, "Test uniform_distribution_cdf_iint32"
        seed = 369147258
        call random_seed(seed, get)
        loc = 14_int32; scale = 100_int32
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_cdf_iint32 failed", warn=warn)
    end subroutine test_uni_cdf_iint32

    subroutine test_uni_cdf_iint64
        integer(int64) :: x1, x2(3,4), loc, scale
        real :: res(3,5)
        integer :: seed, get
        real :: ans(15) = [0.455445558, 0.455445558, 0.455445558, 0.277227730,  &
                           0.455445558, 0.930693090, 0.851485133, 0.623762369,  &
                           5.94059415E-02,0.693069279, 0.544554472, 0.207920790,&
                           0.306930691, 0.356435657, 0.128712878]

        print *, "Test uniform_distribution_cdf_iint64"
        seed = 369147258
        call random_seed(seed, get)
        loc = 14_int64; scale = 100_int64
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_cdf_iint64 failed", warn=warn)
    end subroutine test_uni_cdf_iint64

    subroutine test_uni_cdf_rsp
        real(sp) :: x1, x2(3,4), loc, scale
        real :: res(3,5)
        integer :: seed, get
        real :: ans(15) = [0.170192942, 0.170192942, 0.170192942, 0.276106149,  &
                           0.754930079, 0.406620681, 0.187742814, 0.651605546,  &
                           0.934733927, 0.151271492, 0.987674534, 0.130533904,  &
                           0.106271908, 9.27578658E-02, 0.203399420]

        print *, "Test uniform_distribution_cdf_rsp"
        seed = 369147258
        call random_seed(seed, get)
        loc = 0.0_sp; scale = 2.0_sp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_cdf_rsp failed", warn=warn)
    end subroutine test_uni_cdf_rsp

    subroutine test_uni_cdf_rdp
        real(dp) :: x1, x2(3,4), loc, scale
        real :: res(3,5)
        integer :: seed, get
        real :: ans(15) = [0.170192942, 0.170192942, 0.170192942, 0.276106149,  &
                           0.754930079, 0.406620681, 0.187742814, 0.651605546,  &
                           0.934733927, 0.151271492, 0.987674534, 0.130533904,  &
                           0.106271908, 9.27578658E-02, 0.203399420]

        print *, "Test uniform_distribution_cdf_rdp"
        seed = 369147258
        call random_seed(seed, get)
        loc = 0.0_dp; scale = 2.0_dp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_cdf_rdp failed", warn=warn)
    end subroutine test_uni_cdf_rdp

    subroutine test_uni_cdf_rqp
        real(qp) :: x1, x2(3,4), loc, scale
        real :: res(3,5)
        integer :: seed, get
        real :: ans(15) = [0.170192942, 0.170192942, 0.170192942, 0.276106149,  &
                           0.754930079, 0.406620681, 0.187742814, 0.651605546,  &
                           0.934733927, 0.151271492, 0.987674534, 0.130533904,  &
                           0.106271908, 9.27578658E-02, 0.203399420]

        print *, "Test uniform_distribution_cdf_rqp"
        seed = 369147258
        call random_seed(seed, get)
        loc = 0.0_qp; scale = 2.0_qp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_cdf_rqp failed", warn=warn)
    end subroutine test_uni_cdf_rqp

    subroutine test_uni_cdf_csp
        complex(sp) :: x1, x2(3,4), loc, scale
        real :: res(3,5)
        integer :: seed, get
        real :: ans(15) = [4.69913185E-02, 4.69913185E-02, 4.69913185E-02,      &
                           0.306970179, 0.122334257, 0.141398594, 0.128925011,  &
                           9.85755492E-03, 8.16527531E-02, 0.163921610,         &
                           7.81712309E-02, 0.446415812, 5.31753292E-04,         &
                           0.101455867, 0.155276477]

        print *, "Test uniform_distribution_cdf_csp"
        seed = 369147258
        call random_seed(seed, get)
        loc = (-0.5_sp, -0.5_sp); scale = (1.0_sp, 1.0_sp)
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_cdf_csp failed", warn=warn)
    end subroutine test_uni_cdf_csp

    subroutine test_uni_cdf_cdp
        complex(dp) :: x1, x2(3,4), loc, scale
        real :: res(3,5)
        integer :: seed, get
        real :: ans(15) = [4.69913185E-02, 4.69913185E-02, 4.69913185E-02,      &
                           0.306970179, 0.122334257, 0.141398594, 0.128925011,  &
                           9.85755492E-03, 8.16527531E-02, 0.163921610,         &
                           7.81712309E-02, 0.446415812, 5.31753292E-04,         &
                           0.101455867, 0.155276477]

        print *, "Test uniform_distribution_cdf_cdp"
        seed = 369147258
        call random_seed(seed, get)
        loc = (-0.5_dp, -0.5_dp); scale = (1.0_dp, 1.0_dp)
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_cdf_cdp failed", warn=warn)
    end subroutine test_uni_cdf_cdp

    subroutine test_uni_cdf_cqp
        complex(qp) :: x1, x2(3,4), loc, scale
        real :: res(3,5)
        integer :: seed, get
        real :: ans(15) = [4.69913185E-02, 4.69913185E-02, 4.69913185E-02,      &
                           0.306970179, 0.122334257, 0.141398594, 0.128925011,  &
                           9.85755492E-03, 8.16527531E-02, 0.163921610,         &
                           7.81712309E-02, 0.446415812, 5.31753292E-04,         &
                           0.101455867, 0.155276477]

        print *, "Test uniform_distribution_cdf_cqp"
        seed = 369147258
        call random_seed(seed, get)
        loc = (-0.5_qp, -0.5_qp); scale = (1.0_qp, 1.0_qp)
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_cdf_cqp failed", warn=warn)
    end subroutine test_uni_cdf_cqp


end program test_distribution_uniform