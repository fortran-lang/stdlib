program test_distribution
    use stdlib_stats_distribution,                            &
                 uni_rvs => uniform_distribution_rvs,         &
                 uni_pdf => uniform_distribution_pdf,         &
                 uni_cdf => uniform_distribution_cdf,         &
                 nor_rvs => normal_distribution_rvs,          &
                 nor_pdf => normal_distribution_pdf,          &
                 nor_cdf => normal_distribution_cdf,          &
                 binom_rvs => binomial_distribution_rvs,      &
                 binom_pmf => binomial_distribution_pmf,      &
                 binom_cdf => binomial_distribution_cdf
    use stdlib_error, only : check
    use stdlib_kinds

    implicit none
    real(sp), parameter :: sptol = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1.0_dp)
    real(qp), parameter :: qptol = 1000 * epsilon(1.0_qp)
    logical ::  warn = .true.

    call test_random_seed
    call test_uniform_random_generator
    call test_normal_random_generator
    call test_binomial_random_generator

      call test_uni_rvs_sp_int8
      call test_uni_rvs_sp_int16
      call test_uni_rvs_sp_int32
      call test_uni_rvs_sp_int64
      call test_uni_rvs_dp_int8
      call test_uni_rvs_dp_int16
      call test_uni_rvs_dp_int32
      call test_uni_rvs_dp_int64
      call test_uni_rvs_qp_int8
      call test_uni_rvs_qp_int16
      call test_uni_rvs_qp_int32
      call test_uni_rvs_qp_int64

    call test_uni_pdf_sp
    call test_uni_pdf_dp
    call test_uni_pdf_qp
    call test_uni_cdf_sp
    call test_uni_cdf_dp
    call test_uni_cdf_qp

      call test_nor_rvs_sp_int8
      call test_nor_rvs_sp_int16
      call test_nor_rvs_sp_int32
      call test_nor_rvs_sp_int64
      call test_nor_rvs_dp_int8
      call test_nor_rvs_dp_int16
      call test_nor_rvs_dp_int32
      call test_nor_rvs_dp_int64
      call test_nor_rvs_qp_int8
      call test_nor_rvs_qp_int16
      call test_nor_rvs_qp_int32
      call test_nor_rvs_qp_int64

    call test_nor_pdf_sp
    call test_nor_pdf_dp
    call test_nor_pdf_qp
    call test_nor_cdf_sp
    call test_nor_cdf_dp
    call test_nor_cdf_qp

    call test_binom_rvs_int8
    call test_binom_rvs_int16
    call test_binom_rvs_int32
    call test_binom_rvs_int64
    call test_binom_pmf_int8
    call test_binom_pmf_int16
    call test_binom_pmf_int32
    call test_binom_pmf_int64
    call test_binom_cdf_int8
    call test_binom_cdf_int16
    call test_binom_cdf_int32
    call test_binom_cdf_int64


    contains

    subroutine test_random_seed
        integer(int32) :: put, get, res(5)
        integer(int32) :: ans(5) = [-1859553078, -1933696596,  -642834430, &
                                    1711399314,  1548311463]
        integer :: i

        print *, ""
        print *, "Test random_seed"
        put = 135792468
        do i = 1, 5
            call random_seed(put,get)
            res(i) = get
            put = get
        end do
        call check(all(res == ans), &
          msg="random seed test failed.",warn=warn)
    end subroutine test_random_seed

    subroutine test_uniform_random_generator
        integer :: i, j, freq(0:1000), num=10000000
        real(dp) :: chisq, expct

        print *,""
        print *, "Test uniform random generator with chi-squared"
        freq = 0
        do i = 1, num
            j = 1000 * uni_rvs(0.0, 1.0)
            freq(j) = freq(j) + 1
        end do
        chisq = 0.0_dp
        expct = num / 1000
        do i = 0, 999
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do
        write(*,*) "The critical values for chi-squared with 1000 d. of f. are"
        write(*,*) "867.48 and 1143.92"
        write(*,*) "Chi-squared for uniform random generator is : ", chisq
        call check((chisq < 1143.9 .and. chisq > 867.48) , &
               msg="uniform randomness failed chi-squared test", warn=warn)
    end subroutine test_uniform_random_generator

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
        write(*,*) "The critical values for chi-squared with 1000 d. of f. are"
        write(*,*) "867.48 and 1143.92"
        write(*,*) "Chi-squared for normal random generator is : ", chisq
        call check((chisq < 1143.9 .and. chisq > 867.48), &
               msg="normal randomness failed chi-squared test", warn=warn)
    end subroutine test_normal_random_generator

    subroutine test_binomial_random_generator
        integer :: i, j, n, num=1000000
        real(dp) :: chisq, expct
        real :: p
        integer, allocatable :: freq(:)

        print *, ""
        print *, "Test binomial1 random generator with chi-squared"
        n = 10
        p = 0.45
        allocate(freq(0:n))
        freq = 0
        do i = 1, num
            j = binom_rvs(n, p)
            freq(j) = freq(j) + 1
        end do
        chisq = 0.0_dp
        do i = 0, n
           expct = num * binom_pmf(i, n, p)
           if(expct < 1.0e-5) cycle
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do
        write(*,*) "The critical values for chi-squared with 9 d. of f. are"
        write(*,*) "1.15 and 27.87"
        write(*,*) "Chi-squared for binomial random generator is : ", chisq
        call check((chisq < 27.88 .and. chisq > 1.15), &
             msg="binomial1 randomness failed chi-squared test", warn=warn)
        n = 140
        p = 0.34
        deallocate(freq)
        allocate(freq(0:n))
        freq = 0
        do i = 1, num
           j = binom_rvs(n, p)
           freq(j) = freq(j) + 1
        end do
        chisq = 0.0_dp
        do i = 0, n
           expct = num * binom_pmf(i, n, p)
           if(expct < 1.0e-5) cycle
           chisq = chisq + (freq(i) - expct) ** 2 / expct
        end do
        write(*,*) ""
        write(*,*) "The critical values for chi-squared with 49 d. of f. are"
        write(*,*) "23.98 and 85.35"
        write(*,*) "Chi-squared for binomial random generator is : ", chisq
        call check((chisq < 85.35 .and. chisq > 23.98), &
             msg="binomial1 randomness failed chi-squared test", warn=warn)
    end subroutine test_binomial_random_generator

      subroutine test_uni_rvs_sp_int8
          real(sp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n, seed, get

          real(sp) :: ans(20) =                                            &
                  [0.457413316_sp, 0.183665052_sp, 0.887956202_sp, &
                   0.442960650_sp, 0.475367814_sp, 0.170218706_sp, &
                   0.441669136_sp, 0.918695927_sp, 0.148022801_sp, &
                   0.691296339_sp,-0.132472515_sp,-0.878723383_sp, &
                  -0.901660025_sp,-0.164090633_sp,-0.333886743_sp, &
                   0.119948030_sp,-0.859304368_sp,-0.952883482_sp, &
                   0.854362130_sp, 0.692578673_sp]

          print *, "Test uniform_distribution_rvs_sp_int8"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_sp
          scale = 1.0_sp
          k = 5_int8
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < sptol), &
            msg="uniform_distribution_rvs_sp_int8 failed", warn=warn)
      end subroutine test_uni_rvs_sp_int8

      subroutine test_uni_rvs_sp_int16
          real(sp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n, seed, get

          real(sp) :: ans(20) =                                            &
                  [0.457413316_sp, 0.183665052_sp, 0.887956202_sp, &
                   0.442960650_sp, 0.475367814_sp, 0.170218706_sp, &
                   0.441669136_sp, 0.918695927_sp, 0.148022801_sp, &
                   0.691296339_sp,-0.132472515_sp,-0.878723383_sp, &
                  -0.901660025_sp,-0.164090633_sp,-0.333886743_sp, &
                   0.119948030_sp,-0.859304368_sp,-0.952883482_sp, &
                   0.854362130_sp, 0.692578673_sp]

          print *, "Test uniform_distribution_rvs_sp_int16"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_sp
          scale = 1.0_sp
          k = 5_int16
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < sptol), &
            msg="uniform_distribution_rvs_sp_int16 failed", warn=warn)
      end subroutine test_uni_rvs_sp_int16

      subroutine test_uni_rvs_sp_int32
          real(sp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n, seed, get

          real(sp) :: ans(20) =                                            &
                  [0.457413316_sp, 0.183665052_sp, 0.887956202_sp, &
                   0.442960650_sp, 0.475367814_sp, 0.170218706_sp, &
                   0.441669136_sp, 0.918695927_sp, 0.148022801_sp, &
                   0.691296339_sp,-0.132472515_sp,-0.878723383_sp, &
                  -0.901660025_sp,-0.164090633_sp,-0.333886743_sp, &
                   0.119948030_sp,-0.859304368_sp,-0.952883482_sp, &
                   0.854362130_sp, 0.692578673_sp]

          print *, "Test uniform_distribution_rvs_sp_int32"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_sp
          scale = 1.0_sp
          k = 5_int32
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < sptol), &
            msg="uniform_distribution_rvs_sp_int32 failed", warn=warn)
      end subroutine test_uni_rvs_sp_int32

      subroutine test_uni_rvs_sp_int64
          real(sp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n, seed, get

          real(sp) :: ans(20) =                                            &
                  [0.457413316_sp, 0.183665052_sp, 0.887956202_sp, &
                   0.442960650_sp, 0.475367814_sp, 0.170218706_sp, &
                   0.441669136_sp, 0.918695927_sp, 0.148022801_sp, &
                   0.691296339_sp,-0.132472515_sp,-0.878723383_sp, &
                  -0.901660025_sp,-0.164090633_sp,-0.333886743_sp, &
                   0.119948030_sp,-0.859304368_sp,-0.952883482_sp, &
                   0.854362130_sp, 0.692578673_sp]

          print *, "Test uniform_distribution_rvs_sp_int64"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_sp
          scale = 1.0_sp
          k = 5_int64
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < sptol), &
            msg="uniform_distribution_rvs_sp_int64 failed", warn=warn)
      end subroutine test_uni_rvs_sp_int64

      subroutine test_uni_rvs_dp_int8
          real(dp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n, seed, get

          real(dp) :: ans(20) =                                            &
                      [0.45741331426937459_dp, 0.18366504933248320_dp, &
                       0.88795621528854640_dp, 0.44296065449379507_dp, &
                       0.47536782827149393_dp, 0.17021871307147243_dp, &
                       0.44166914074652641_dp, 0.91869594694692958_dp, &
                       0.14802280170069973_dp, 0.69129636492557078_dp, &
                      -0.13247249397818517_dp,-0.87872336629421621_dp, &
                      -0.90166004614151585_dp,-0.16409061414773740_dp, &
                      -0.33388671819038429_dp, 0.11994803941701271_dp, &
                      -0.85930438039845658_dp,-0.95288345131793517_dp, &
                       0.85436218295364763_dp, 0.69257869952149043_dp]


          print *, "Test uniform_distribution_rvs_dp_int8"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_dp
          scale = 1.0_dp
          k = 5_int8
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < dptol), &
            msg="uniform_distribution_rvs_dp_int8 failed", warn=warn)
      end subroutine test_uni_rvs_dp_int8

      subroutine test_uni_rvs_dp_int16
          real(dp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n, seed, get

          real(dp) :: ans(20) =                                            &
                      [0.45741331426937459_dp, 0.18366504933248320_dp, &
                       0.88795621528854640_dp, 0.44296065449379507_dp, &
                       0.47536782827149393_dp, 0.17021871307147243_dp, &
                       0.44166914074652641_dp, 0.91869594694692958_dp, &
                       0.14802280170069973_dp, 0.69129636492557078_dp, &
                      -0.13247249397818517_dp,-0.87872336629421621_dp, &
                      -0.90166004614151585_dp,-0.16409061414773740_dp, &
                      -0.33388671819038429_dp, 0.11994803941701271_dp, &
                      -0.85930438039845658_dp,-0.95288345131793517_dp, &
                       0.85436218295364763_dp, 0.69257869952149043_dp]


          print *, "Test uniform_distribution_rvs_dp_int16"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_dp
          scale = 1.0_dp
          k = 5_int16
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < dptol), &
            msg="uniform_distribution_rvs_dp_int16 failed", warn=warn)
      end subroutine test_uni_rvs_dp_int16

      subroutine test_uni_rvs_dp_int32
          real(dp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n, seed, get

          real(dp) :: ans(20) =                                            &
                      [0.45741331426937459_dp, 0.18366504933248320_dp, &
                       0.88795621528854640_dp, 0.44296065449379507_dp, &
                       0.47536782827149393_dp, 0.17021871307147243_dp, &
                       0.44166914074652641_dp, 0.91869594694692958_dp, &
                       0.14802280170069973_dp, 0.69129636492557078_dp, &
                      -0.13247249397818517_dp,-0.87872336629421621_dp, &
                      -0.90166004614151585_dp,-0.16409061414773740_dp, &
                      -0.33388671819038429_dp, 0.11994803941701271_dp, &
                      -0.85930438039845658_dp,-0.95288345131793517_dp, &
                       0.85436218295364763_dp, 0.69257869952149043_dp]


          print *, "Test uniform_distribution_rvs_dp_int32"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_dp
          scale = 1.0_dp
          k = 5_int32
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < dptol), &
            msg="uniform_distribution_rvs_dp_int32 failed", warn=warn)
      end subroutine test_uni_rvs_dp_int32

      subroutine test_uni_rvs_dp_int64
          real(dp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n, seed, get

          real(dp) :: ans(20) =                                            &
                      [0.45741331426937459_dp, 0.18366504933248320_dp, &
                       0.88795621528854640_dp, 0.44296065449379507_dp, &
                       0.47536782827149393_dp, 0.17021871307147243_dp, &
                       0.44166914074652641_dp, 0.91869594694692958_dp, &
                       0.14802280170069973_dp, 0.69129636492557078_dp, &
                      -0.13247249397818517_dp,-0.87872336629421621_dp, &
                      -0.90166004614151585_dp,-0.16409061414773740_dp, &
                      -0.33388671819038429_dp, 0.11994803941701271_dp, &
                      -0.85930438039845658_dp,-0.95288345131793517_dp, &
                       0.85436218295364763_dp, 0.69257869952149043_dp]


          print *, "Test uniform_distribution_rvs_dp_int64"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_dp
          scale = 1.0_dp
          k = 5_int64
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < dptol), &
            msg="uniform_distribution_rvs_dp_int64 failed", warn=warn)
      end subroutine test_uni_rvs_dp_int64

      subroutine test_uni_rvs_qp_int8
          real(qp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n, seed, get

          real(qp) :: ans(20) =                                       &
                          [0.457413314269374593479255963757168502_qp, &
                           0.183665049332483204524990583195176441_qp, &
                           0.887956215288546402142344504682114348_qp, &
                           0.442960654493795069619466175936395302_qp, &
                           0.475367828271493930714086673106066883_qp, &
                           0.170218713071472432796227280960010830_qp, &
                           0.441669140746526411867023398372111842_qp, &
                           0.918695946946929575815943280758801848_qp, &
                           0.148022801700699729865462472844228614_qp, &
                           0.691296364925570783199759716808330268_qp, &
                          -0.132472493978185168472805344208609313_qp, &
                          -0.878723366294216184924081858298450243_qp, &
                          -0.901660046141515819639877804547722917_qp, &
                          -0.164090614147737401395943379611708224_qp, &
                          -0.333886718190384290672056977200554684_qp, &
                           0.119948039417012708440779533702880144_qp, &
                          -0.859304380398456552070385328079282772_qp, &
                          -0.952883451317935156743565983106236672_qp, &
                           0.854362182953647630867521911568474025_qp, &
                           0.692578699521490426249670235847588629_qp]


          print *, "Test uniform_distribution_rvs_qp_int8"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_qp
          scale = 1.0_qp
          k = 5_int8
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < qptol), &
            msg="uniform_distribution_rvs_qp_int8 failed", warn=warn)
      end subroutine test_uni_rvs_qp_int8

      subroutine test_uni_rvs_qp_int16
          real(qp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n, seed, get

          real(qp) :: ans(20) =                                       &
                          [0.457413314269374593479255963757168502_qp, &
                           0.183665049332483204524990583195176441_qp, &
                           0.887956215288546402142344504682114348_qp, &
                           0.442960654493795069619466175936395302_qp, &
                           0.475367828271493930714086673106066883_qp, &
                           0.170218713071472432796227280960010830_qp, &
                           0.441669140746526411867023398372111842_qp, &
                           0.918695946946929575815943280758801848_qp, &
                           0.148022801700699729865462472844228614_qp, &
                           0.691296364925570783199759716808330268_qp, &
                          -0.132472493978185168472805344208609313_qp, &
                          -0.878723366294216184924081858298450243_qp, &
                          -0.901660046141515819639877804547722917_qp, &
                          -0.164090614147737401395943379611708224_qp, &
                          -0.333886718190384290672056977200554684_qp, &
                           0.119948039417012708440779533702880144_qp, &
                          -0.859304380398456552070385328079282772_qp, &
                          -0.952883451317935156743565983106236672_qp, &
                           0.854362182953647630867521911568474025_qp, &
                           0.692578699521490426249670235847588629_qp]


          print *, "Test uniform_distribution_rvs_qp_int16"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_qp
          scale = 1.0_qp
          k = 5_int16
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < qptol), &
            msg="uniform_distribution_rvs_qp_int16 failed", warn=warn)
      end subroutine test_uni_rvs_qp_int16

      subroutine test_uni_rvs_qp_int32
          real(qp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n, seed, get

          real(qp) :: ans(20) =                                       &
                          [0.457413314269374593479255963757168502_qp, &
                           0.183665049332483204524990583195176441_qp, &
                           0.887956215288546402142344504682114348_qp, &
                           0.442960654493795069619466175936395302_qp, &
                           0.475367828271493930714086673106066883_qp, &
                           0.170218713071472432796227280960010830_qp, &
                           0.441669140746526411867023398372111842_qp, &
                           0.918695946946929575815943280758801848_qp, &
                           0.148022801700699729865462472844228614_qp, &
                           0.691296364925570783199759716808330268_qp, &
                          -0.132472493978185168472805344208609313_qp, &
                          -0.878723366294216184924081858298450243_qp, &
                          -0.901660046141515819639877804547722917_qp, &
                          -0.164090614147737401395943379611708224_qp, &
                          -0.333886718190384290672056977200554684_qp, &
                           0.119948039417012708440779533702880144_qp, &
                          -0.859304380398456552070385328079282772_qp, &
                          -0.952883451317935156743565983106236672_qp, &
                           0.854362182953647630867521911568474025_qp, &
                           0.692578699521490426249670235847588629_qp]


          print *, "Test uniform_distribution_rvs_qp_int32"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_qp
          scale = 1.0_qp
          k = 5_int32
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < qptol), &
            msg="uniform_distribution_rvs_qp_int32 failed", warn=warn)
      end subroutine test_uni_rvs_qp_int32

      subroutine test_uni_rvs_qp_int64
          real(qp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n, seed, get

          real(qp) :: ans(20) =                                       &
                          [0.457413314269374593479255963757168502_qp, &
                           0.183665049332483204524990583195176441_qp, &
                           0.887956215288546402142344504682114348_qp, &
                           0.442960654493795069619466175936395302_qp, &
                           0.475367828271493930714086673106066883_qp, &
                           0.170218713071472432796227280960010830_qp, &
                           0.441669140746526411867023398372111842_qp, &
                           0.918695946946929575815943280758801848_qp, &
                           0.148022801700699729865462472844228614_qp, &
                           0.691296364925570783199759716808330268_qp, &
                          -0.132472493978185168472805344208609313_qp, &
                          -0.878723366294216184924081858298450243_qp, &
                          -0.901660046141515819639877804547722917_qp, &
                          -0.164090614147737401395943379611708224_qp, &
                          -0.333886718190384290672056977200554684_qp, &
                           0.119948039417012708440779533702880144_qp, &
                          -0.859304380398456552070385328079282772_qp, &
                          -0.952883451317935156743565983106236672_qp, &
                           0.854362182953647630867521911568474025_qp, &
                           0.692578699521490426249670235847588629_qp]


          print *, "Test uniform_distribution_rvs_qp_int64"
          seed = 258147369
          call random_seed(seed, get)
          loc = 0.0_qp
          scale = 1.0_qp
          k = 5_int64
          do i = 1, 5
            res(i) = uni_rvs(loc, scale)
          end do
          res(6:10) = uni_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = uni_rvs(loc, scale)
          end do
          res(16:20) = uni_rvs(loc, scale, k)
          call check(all(abs(res - ans) < qptol), &
            msg="uniform_distribution_rvs_qp_int64 failed", warn=warn)
      end subroutine test_uni_rvs_qp_int64


    subroutine test_uni_pdf_sp
        real(sp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(sp) :: ans(15) = [(0.2_sp, i=1,15)]


        print *, "Test uniform_distribution_pdf_sp"
        seed = 147258639
        call random_seed(seed, get)
        loc = 0.0_sp
        scale = 5.0_sp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_pdf_sp failed", warn=warn)
    end subroutine test_uni_pdf_sp

    subroutine test_uni_pdf_dp
        real(dp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(dp) :: ans(15) = [(0.2_dp, i=1,15)]


        print *, "Test uniform_distribution_pdf_dp"
        seed = 147258639
        call random_seed(seed, get)
        loc = 0.0_dp
        scale = 5.0_dp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol), &
            msg="uniform_distribution_pdf_dp failed", warn=warn)
    end subroutine test_uni_pdf_dp

    subroutine test_uni_pdf_qp
        real(qp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(qp) :: ans(15) = [(0.2_qp, i=1,15)]


        print *, "Test uniform_distribution_pdf_qp"
        seed = 147258639
        call random_seed(seed, get)
        loc = 0.0_qp
        scale = 5.0_qp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < qptol), &
            msg="uniform_distribution_pdf_qp failed", warn=warn)
    end subroutine test_uni_pdf_qp


    subroutine test_uni_cdf_sp
        real(sp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(sp) :: ans(15) =                                              &
                  [0.170192957_sp, 0.170192957_sp, 0.170192957_sp, &
                   0.276106149_sp, 0.754930079_sp, 0.406620681_sp, &
                   0.187742829_sp, 0.651605546_sp, 0.934733927_sp, &
                   0.151271492_sp, 0.987674534_sp, 0.130533904_sp, &
                 0.106271923_sp, 9.27578807E-02_sp, 0.203399420_sp]

        print *, "Test uniform_distribution_cdf_sp"
        seed = 369147258
        call random_seed(seed, get)
        loc = -1.0_sp
        scale = 2.0_sp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="uniform_distribution_cdf_sp failed", warn=warn)
    end subroutine test_uni_cdf_sp

    subroutine test_uni_cdf_dp
        real(dp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(dp) :: ans(15) =                                              &
                      [0.17019294429755738_dp, 0.17019294429755738_dp, &
                       0.17019294429755738_dp, 0.27610614627464619_dp, &
                       0.75493009747387507_dp, 0.40662068257311801_dp, &
                       0.18774281919180108_dp, 0.65160552609050759_dp, &
                       0.93473394973210489_dp, 0.15127149185161326_dp, &
                       0.98767452279771961_dp, 0.13053389946340466_dp, &
                    0.10627190592187685_dp, 9.2757865224011304E-002_dp,&
                       0.20339942685342044_dp]

        print *, "Test uniform_distribution_cdf_dp"
        seed = 369147258
        call random_seed(seed, get)
        loc = -1.0_dp
        scale = 2.0_dp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol), &
            msg="uniform_distribution_cdf_dp failed", warn=warn)
    end subroutine test_uni_cdf_dp

    subroutine test_uni_cdf_qp
        real(qp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(qp) :: ans(15) =                                              &
                               [0.170192944297557408050991512027394492_qp, &
                                0.170192944297557408050991512027394492_qp, &
                                0.170192944297557408050991512027394492_qp, &
                                0.276106146274646191418611351764411665_qp, &
                                0.754930097473875072466853453079238534_qp, &
                                0.406620682573118008562573777453508228_qp, &
                                0.187742819191801080247472555129206739_qp, &
                                0.651605526090507591874256831943057477_qp, &
                                0.934733949732104885121941606485052034_qp, &
                                0.151271491851613287815681019310432021_qp, &
                                0.987674522797719611766353864368284121_qp, &
                                0.130533899463404684526679488953959662_qp, &
                                0.106271905921876880229959283497009892_qp, &
                            9.27578652240113182836367400341259781E-0002_qp,&
                                0.203399426853420439709196898547816090_qp]

        print *, "Test uniform_distribution_cdf_qp"
        seed = 369147258
        call random_seed(seed, get)
        loc = -1.0_qp
        scale = 2.0_qp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < qptol), &
            msg="uniform_distribution_cdf_qp failed", warn=warn)
    end subroutine test_uni_cdf_qp


      subroutine test_nor_rvs_sp_int8
          real(sp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n
          integer :: seed, get
          real(sp) :: ans(20) =                                            &
                   [1.08354020_sp, 0.930153966_sp, 0.388561100_sp, &
                   -1.44566274_sp, 0.612832963_sp, -1.00310886_sp, &
                  0.817594171_sp, -0.568394125_sp, 0.993003964_sp, &
                   -1.76855731_sp, -3.22572017_sp, -3.99702096_sp, &
                   0.428807259_sp, -2.44686961_sp, -3.48141479_sp, &
                  -4.33496284_sp, -0.154625356_sp,-0.830695271_sp, &
                  -3.90960717_sp, 1.53445792_sp]

          print *, "Test normal_distribution_rvs_sp_int8"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_sp
          scale = 1.0_sp
          k = 5_int8
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < sptol), &
            msg="normal_distribution_rvs_sp_int8 failed", warn=warn)
      end subroutine test_nor_rvs_sp_int8

      subroutine test_nor_rvs_sp_int16
          real(sp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n
          integer :: seed, get
          real(sp) :: ans(20) =                                            &
                   [1.08354020_sp, 0.930153966_sp, 0.388561100_sp, &
                   -1.44566274_sp, 0.612832963_sp, -1.00310886_sp, &
                  0.817594171_sp, -0.568394125_sp, 0.993003964_sp, &
                   -1.76855731_sp, -3.22572017_sp, -3.99702096_sp, &
                   0.428807259_sp, -2.44686961_sp, -3.48141479_sp, &
                  -4.33496284_sp, -0.154625356_sp,-0.830695271_sp, &
                  -3.90960717_sp, 1.53445792_sp]

          print *, "Test normal_distribution_rvs_sp_int16"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_sp
          scale = 1.0_sp
          k = 5_int16
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < sptol), &
            msg="normal_distribution_rvs_sp_int16 failed", warn=warn)
      end subroutine test_nor_rvs_sp_int16

      subroutine test_nor_rvs_sp_int32
          real(sp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n
          integer :: seed, get
          real(sp) :: ans(20) =                                            &
                   [1.08354020_sp, 0.930153966_sp, 0.388561100_sp, &
                   -1.44566274_sp, 0.612832963_sp, -1.00310886_sp, &
                  0.817594171_sp, -0.568394125_sp, 0.993003964_sp, &
                   -1.76855731_sp, -3.22572017_sp, -3.99702096_sp, &
                   0.428807259_sp, -2.44686961_sp, -3.48141479_sp, &
                  -4.33496284_sp, -0.154625356_sp,-0.830695271_sp, &
                  -3.90960717_sp, 1.53445792_sp]

          print *, "Test normal_distribution_rvs_sp_int32"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_sp
          scale = 1.0_sp
          k = 5_int32
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < sptol), &
            msg="normal_distribution_rvs_sp_int32 failed", warn=warn)
      end subroutine test_nor_rvs_sp_int32

      subroutine test_nor_rvs_sp_int64
          real(sp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n
          integer :: seed, get
          real(sp) :: ans(20) =                                            &
                   [1.08354020_sp, 0.930153966_sp, 0.388561100_sp, &
                   -1.44566274_sp, 0.612832963_sp, -1.00310886_sp, &
                  0.817594171_sp, -0.568394125_sp, 0.993003964_sp, &
                   -1.76855731_sp, -3.22572017_sp, -3.99702096_sp, &
                   0.428807259_sp, -2.44686961_sp, -3.48141479_sp, &
                  -4.33496284_sp, -0.154625356_sp,-0.830695271_sp, &
                  -3.90960717_sp, 1.53445792_sp]

          print *, "Test normal_distribution_rvs_sp_int64"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_sp
          scale = 1.0_sp
          k = 5_int64
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < sptol), &
            msg="normal_distribution_rvs_sp_int64 failed", warn=warn)
      end subroutine test_nor_rvs_sp_int64

      subroutine test_nor_rvs_dp_int8
          real(dp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n
          integer :: seed, get
          real(dp) :: ans(20) =                                            &
                       [1.0835401965902034_dp, 0.93015397468064165_dp, &
                       0.38856109396542121_dp, -1.4456627206540740_dp, &
                       0.61283297553014326_dp, -1.0031088776888382_dp, &
                      0.81759417579176041_dp, -0.56839412687107116_dp, &
                       0.99300393889422900_dp, -1.7685573691899061_dp, &
                       -3.2257201976639149_dp, -3.9970210500520191_dp, &
                       0.42880723775826013_dp, -2.4468696145147510_dp, &
                       -3.4814147687231882_dp, -4.3349631940225235_dp, &
                     -0.15462537592816106_dp, -0.83069527405234211_dp, &
                      -3.9096071456063441_dp, 1.5344578674016103_dp]

          print *, "Test normal_distribution_rvs_dp_int8"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_dp
          scale = 1.0_dp
          k = 5_int8
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < dptol), &
            msg="normal_distribution_rvs_dp_int8 failed", warn=warn)
      end subroutine test_nor_rvs_dp_int8

      subroutine test_nor_rvs_dp_int16
          real(dp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n
          integer :: seed, get
          real(dp) :: ans(20) =                                            &
                       [1.0835401965902034_dp, 0.93015397468064165_dp, &
                       0.38856109396542121_dp, -1.4456627206540740_dp, &
                       0.61283297553014326_dp, -1.0031088776888382_dp, &
                      0.81759417579176041_dp, -0.56839412687107116_dp, &
                       0.99300393889422900_dp, -1.7685573691899061_dp, &
                       -3.2257201976639149_dp, -3.9970210500520191_dp, &
                       0.42880723775826013_dp, -2.4468696145147510_dp, &
                       -3.4814147687231882_dp, -4.3349631940225235_dp, &
                     -0.15462537592816106_dp, -0.83069527405234211_dp, &
                      -3.9096071456063441_dp, 1.5344578674016103_dp]

          print *, "Test normal_distribution_rvs_dp_int16"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_dp
          scale = 1.0_dp
          k = 5_int16
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < dptol), &
            msg="normal_distribution_rvs_dp_int16 failed", warn=warn)
      end subroutine test_nor_rvs_dp_int16

      subroutine test_nor_rvs_dp_int32
          real(dp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n
          integer :: seed, get
          real(dp) :: ans(20) =                                            &
                       [1.0835401965902034_dp, 0.93015397468064165_dp, &
                       0.38856109396542121_dp, -1.4456627206540740_dp, &
                       0.61283297553014326_dp, -1.0031088776888382_dp, &
                      0.81759417579176041_dp, -0.56839412687107116_dp, &
                       0.99300393889422900_dp, -1.7685573691899061_dp, &
                       -3.2257201976639149_dp, -3.9970210500520191_dp, &
                       0.42880723775826013_dp, -2.4468696145147510_dp, &
                       -3.4814147687231882_dp, -4.3349631940225235_dp, &
                     -0.15462537592816106_dp, -0.83069527405234211_dp, &
                      -3.9096071456063441_dp, 1.5344578674016103_dp]

          print *, "Test normal_distribution_rvs_dp_int32"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_dp
          scale = 1.0_dp
          k = 5_int32
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < dptol), &
            msg="normal_distribution_rvs_dp_int32 failed", warn=warn)
      end subroutine test_nor_rvs_dp_int32

      subroutine test_nor_rvs_dp_int64
          real(dp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n
          integer :: seed, get
          real(dp) :: ans(20) =                                            &
                       [1.0835401965902034_dp, 0.93015397468064165_dp, &
                       0.38856109396542121_dp, -1.4456627206540740_dp, &
                       0.61283297553014326_dp, -1.0031088776888382_dp, &
                      0.81759417579176041_dp, -0.56839412687107116_dp, &
                       0.99300393889422900_dp, -1.7685573691899061_dp, &
                       -3.2257201976639149_dp, -3.9970210500520191_dp, &
                       0.42880723775826013_dp, -2.4468696145147510_dp, &
                       -3.4814147687231882_dp, -4.3349631940225235_dp, &
                     -0.15462537592816106_dp, -0.83069527405234211_dp, &
                      -3.9096071456063441_dp, 1.5344578674016103_dp]

          print *, "Test normal_distribution_rvs_dp_int64"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_dp
          scale = 1.0_dp
          k = 5_int64
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < dptol), &
            msg="normal_distribution_rvs_dp_int64 failed", warn=warn)
      end subroutine test_nor_rvs_dp_int64

      subroutine test_nor_rvs_qp_int8
          real(qp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n
          integer :: seed, get
          real(qp) :: ans(20) =                                            &
                                [1.08354019659020339716448688704986125_qp, &
                                0.930153974680641648653534048207802698_qp, &
                                0.388561093965421211482436092410353012_qp, &
                                -1.44566272065407397384717569366330281_qp, &
                                0.612832975530143264641935729741817340_qp, &
                                -1.00310887768883816306697553955018520_qp, &
                                0.817594175791760413574138510739430785_qp, &
                               -0.568394126871071159179393816884839907_qp, &
                                0.993003938894228999068047869513975456_qp, &
                                -1.76855736918990613659730115614365786_qp, &
                                -3.22572019766391493433843606908340007_qp, &
                                -3.99702105005201913101586796983610839_qp, &
                                0.428807237758260129112386493943631649_qp, &
                                -2.44686961451475104567521157150622457_qp, &
                                -3.48141476872318822444185570930130780_qp, &
                                -4.33496319402252350272419789689593017_qp, &
                               -0.154625375928161057359488950169179589_qp, &
                               -0.830695274052342114146085805259644985_qp, &
                                -3.90960714560634414738160558044910431_qp, &
                                 1.53445786740161027594808729190845042_qp]

          print *, "Test normal_distribution_rvs_qp_int8"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_qp
          scale = 1.0_qp
          k = 5_int8
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < qptol), &
            msg="normal_distribution_rvs_qp_int8 failed", warn=warn)
      end subroutine test_nor_rvs_qp_int8

      subroutine test_nor_rvs_qp_int16
          real(qp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n
          integer :: seed, get
          real(qp) :: ans(20) =                                            &
                                [1.08354019659020339716448688704986125_qp, &
                                0.930153974680641648653534048207802698_qp, &
                                0.388561093965421211482436092410353012_qp, &
                                -1.44566272065407397384717569366330281_qp, &
                                0.612832975530143264641935729741817340_qp, &
                                -1.00310887768883816306697553955018520_qp, &
                                0.817594175791760413574138510739430785_qp, &
                               -0.568394126871071159179393816884839907_qp, &
                                0.993003938894228999068047869513975456_qp, &
                                -1.76855736918990613659730115614365786_qp, &
                                -3.22572019766391493433843606908340007_qp, &
                                -3.99702105005201913101586796983610839_qp, &
                                0.428807237758260129112386493943631649_qp, &
                                -2.44686961451475104567521157150622457_qp, &
                                -3.48141476872318822444185570930130780_qp, &
                                -4.33496319402252350272419789689593017_qp, &
                               -0.154625375928161057359488950169179589_qp, &
                               -0.830695274052342114146085805259644985_qp, &
                                -3.90960714560634414738160558044910431_qp, &
                                 1.53445786740161027594808729190845042_qp]

          print *, "Test normal_distribution_rvs_qp_int16"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_qp
          scale = 1.0_qp
          k = 5_int16
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < qptol), &
            msg="normal_distribution_rvs_qp_int16 failed", warn=warn)
      end subroutine test_nor_rvs_qp_int16

      subroutine test_nor_rvs_qp_int32
          real(qp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n
          integer :: seed, get
          real(qp) :: ans(20) =                                            &
                                [1.08354019659020339716448688704986125_qp, &
                                0.930153974680641648653534048207802698_qp, &
                                0.388561093965421211482436092410353012_qp, &
                                -1.44566272065407397384717569366330281_qp, &
                                0.612832975530143264641935729741817340_qp, &
                                -1.00310887768883816306697553955018520_qp, &
                                0.817594175791760413574138510739430785_qp, &
                               -0.568394126871071159179393816884839907_qp, &
                                0.993003938894228999068047869513975456_qp, &
                                -1.76855736918990613659730115614365786_qp, &
                                -3.22572019766391493433843606908340007_qp, &
                                -3.99702105005201913101586796983610839_qp, &
                                0.428807237758260129112386493943631649_qp, &
                                -2.44686961451475104567521157150622457_qp, &
                                -3.48141476872318822444185570930130780_qp, &
                                -4.33496319402252350272419789689593017_qp, &
                               -0.154625375928161057359488950169179589_qp, &
                               -0.830695274052342114146085805259644985_qp, &
                                -3.90960714560634414738160558044910431_qp, &
                                 1.53445786740161027594808729190845042_qp]

          print *, "Test normal_distribution_rvs_qp_int32"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_qp
          scale = 1.0_qp
          k = 5_int32
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < qptol), &
            msg="normal_distribution_rvs_qp_int32 failed", warn=warn)
      end subroutine test_nor_rvs_qp_int32

      subroutine test_nor_rvs_qp_int64
          real(qp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n
          integer :: seed, get
          real(qp) :: ans(20) =                                            &
                                [1.08354019659020339716448688704986125_qp, &
                                0.930153974680641648653534048207802698_qp, &
                                0.388561093965421211482436092410353012_qp, &
                                -1.44566272065407397384717569366330281_qp, &
                                0.612832975530143264641935729741817340_qp, &
                                -1.00310887768883816306697553955018520_qp, &
                                0.817594175791760413574138510739430785_qp, &
                               -0.568394126871071159179393816884839907_qp, &
                                0.993003938894228999068047869513975456_qp, &
                                -1.76855736918990613659730115614365786_qp, &
                                -3.22572019766391493433843606908340007_qp, &
                                -3.99702105005201913101586796983610839_qp, &
                                0.428807237758260129112386493943631649_qp, &
                                -2.44686961451475104567521157150622457_qp, &
                                -3.48141476872318822444185570930130780_qp, &
                                -4.33496319402252350272419789689593017_qp, &
                               -0.154625375928161057359488950169179589_qp, &
                               -0.830695274052342114146085805259644985_qp, &
                                -3.90960714560634414738160558044910431_qp, &
                                 1.53445786740161027594808729190845042_qp]

          print *, "Test normal_distribution_rvs_qp_int64"
          seed = 25836914
          call random_seed(seed, get)
          loc = 0.0_qp
          scale = 1.0_qp
          k = 5_int64
          do i = 1, 5
            res(i) = nor_rvs(loc, scale)
          end do
          res(6:10) = nor_rvs(loc, scale, k)
          loc = -1.0
          scale = 2.0
          do i = 11, 15
              res(i) = nor_rvs(loc, scale)
          end do
          res(16:20) = nor_rvs(loc, scale, k)
          call check(all(abs(res - ans) < qptol), &
            msg="normal_distribution_rvs_qp_int64 failed", warn=warn)
      end subroutine test_nor_rvs_qp_int64



    subroutine test_nor_pdf_sp
        real(sp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(sp) :: ans(15) =                                              &
                  [0.322576135_sp, 0.322576135_sp, 0.322576135_sp, &
                0.300806433_sp, 8.49242210E-02_sp, 0.358480453_sp, &
                   0.398903936_sp, 0.393221349_sp, 0.374799609_sp, &
                5.98081723E-02_sp, 0.398853570_sp, 0.241967395_sp, &
                   0.373766601_sp, 0.356140822_sp, 0.233544141_sp]

        print *, "Test normal_distribution_pdf_sp"
        seed = 741852963
        call random_seed(seed, get)
        loc = 0.0_sp
        scale = 1.0_sp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="normal_distribution_pdf_sp failed", warn=warn)
    end subroutine test_nor_pdf_sp

    subroutine test_nor_pdf_dp
        real(dp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(dp) :: ans(15) =                                              &
                      [0.32257615048492366_dp, 0.32257615048492366_dp, &
                       0.32257615048492366_dp, 0.30080644003932094_dp, &
                   8.4924229110490274E-002_dp, 0.35848043641803234_dp, &
                       0.39890395411791441_dp, 0.39322133798111997_dp, &
                   0.37479961337242840_dp, 5.9808167624805800E-002_dp, &
                       0.39885355470530021_dp, 0.24196740475597517_dp, &
                       0.37376661053141419_dp, 0.35614082586331985_dp, &
                       0.23354412957618306_dp]

        print *, "Test normal_distribution_pdf_dp"
        seed = 741852963
        call random_seed(seed, get)
        loc = 0.0_dp
        scale = 1.0_dp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < dptol), &
            msg="normal_distribution_pdf_dp failed", warn=warn)
    end subroutine test_nor_pdf_dp

    subroutine test_nor_pdf_qp
        real(qp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(qp) :: ans(15) =                                              &
                               [0.322576150484923624816177827114417878_qp, &
                                0.322576150484923624816177827114417878_qp, &
                                0.322576150484923624816177827114417878_qp, &
                                0.300806440039320895258949727681658841_qp, &
                           8.49242291104902651552862536156033805E-0002_qp, &
                                0.358480436418032272301373539277868731_qp, &
                                0.398903954117914366544457439174698638_qp, &
                                0.393221337981119941663547835562354415_qp, &
                                0.374799613372428368299981031485342040_qp, &
                           5.98081676248057976816689135970544796E-0002_qp, &
                                0.398853554705300200346860042267642541_qp, &
                                0.241967404755975138058416435199686964_qp, &
                                0.373766610531414167998075638655556567_qp, &
                                0.356140825863319809711905710918457326_qp, &
                                0.233544129576183026277279390942135717_qp]

        print *, "Test normal_distribution_pdf_qp"
        seed = 741852963
        call random_seed(seed, get)
        loc = 0.0_qp
        scale = 1.0_qp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans, [3,5])) < qptol), &
            msg="normal_distribution_pdf_qp failed", warn=warn)
    end subroutine test_nor_pdf_qp


    subroutine test_nor_cdf_sp
        real(sp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(sp) :: ans(15) =                                              &
         [7.50826299E-02_sp, 7.50826299E-02_sp, 7.50826299E-02_sp, &
                   0.143119842_sp, 0.241425395_sp, 0.284345925_sp, &
                   0.233239830_sp, 0.341059506_sp, 0.353156865_sp, &
             6.81066811E-02_sp, 4.38792408E-02_sp, 0.763679624_sp, &
                   0.363722205_sp, 0.868187129_sp, 0.626506805_sp]

        print *, "Test normal_distribution_cdf_sp"
        seed = 369147582
        call random_seed(seed, get)
        loc = -1.0_sp
        scale = 2.0_sp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < sptol), &
            msg="normal_distribution_cdf_sp failed", warn=warn)
    end subroutine test_nor_cdf_sp

    subroutine test_nor_cdf_dp
        real(dp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(dp) :: ans(15) =                                              &
              [7.5082630503844117E-002_dp, 7.5082630503844117E-002_dp, &
                   7.5082630503844117E-002_dp, 0.14311983410871804_dp, &
                       0.24142542152570318_dp, 0.28434587862603933_dp, &
                       0.23323983636601592_dp, 0.34105950613721914_dp, &
                   0.35315685019983512_dp, 6.8106676639663855E-002_dp, &
                   4.3879233144168306E-002_dp, 0.76367963788286075_dp, &
                       0.36372218758735508_dp, 0.86818711488498046_dp, &
                       0.62650679980965285_dp]

        print *, "Test normal_distribution_cdf_dp"
        seed = 369147582
        call random_seed(seed, get)
        loc = -1.0_dp
        scale = 2.0_dp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < dptol), &
            msg="normal_distribution_cdf_dp failed", warn=warn)
    end subroutine test_nor_cdf_dp

    subroutine test_nor_cdf_qp
        real(qp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer :: seed, get
        real(qp) :: ans(15) =                                              &
                          [7.50826305038441048487991102776954069E-0002_qp, &
                           7.50826305038441048487991102776954069E-0002_qp, &
                           7.50826305038441048487991102776954069E-0002_qp, &
                                0.143119834108717983250834016885129863_qp, &
                                0.241425421525703182028420560765471735_qp, &
                                0.284345878626039240974266199229875972_qp, &
                                0.233239836366015928845367994433532733_qp, &
                                0.341059506137219171082517155967522896_qp, &
                                0.353156850199835111081038166086606192_qp, &
                           6.81066766396638231790017005897813364E-0002_qp, &
                           4.38792331441682923984716366123285768E-0002_qp, &
                                0.763679637882860826030745070304416929_qp, &
                                0.363722187587355040667876190724308059_qp, &
                                0.868187114884980488672309198087692444_qp, &
                                0.626506799809652872401992867475200722_qp]

        print *, "Test normal_distribution_cdf_qp"
        seed = 369147582
        call random_seed(seed, get)
        loc = -1.0_qp
        scale = 2.0_qp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(abs(res - reshape(ans,[3,5])) < qptol), &
            msg="normal_distribution_cdf_qp failed", warn=warn)
    end subroutine test_nor_cdf_qp


      subroutine test_binom_rvs_int8
          integer(int8) :: res(40), k, n
          integer :: i, m
          real :: p
          integer :: seed, get
          integer(int8) :: ans(40) =                                                 &
                      [71_int8, 81_int8, 75_int8, 78_int8, 74_int8, &
                       77_int8, 81_int8, 80_int8, 81_int8, 78_int8, &
                       10_int8,  5_int8,  6_int8, 12_int8, 10_int8, &
                       10_int8, 10_int8, 11_int8, 11_int8,  9_int8, &
                       28_int8, 27_int8, 26_int8, 28_int8, 19_int8, &
                       20_int8, 29_int8, 25_int8, 23_int8, 27_int8, &
                        6_int8,  4_int8,  5_int8,  5_int8,  7_int8, &
                        8_int8,  6_int8,  7_int8,  8_int8,  7_int8]

          print *, "Test binomial_distribution_rvs_int8"
          seed = 852693417
          call random_seed(seed, get)
          n = 100_int8
          p = 0.76
          k = 5_int8
          do i = 1, 5
            res(i) = binom_rvs(n, p)
          end do
          res(6:10) = binom_rvs(n, p, k)
          n = 20_int8
          p = 0.4
          do i = 11, 15
              res(i) = binom_rvs(n, P)
          end do
          res(16:20) = binom_rvs(n, p, k)

          n = 100_int8
          p = 0.26
          k = 5_int8
          do i = 21, 25
            res(i) = binom_rvs(n, p)
          end do
          res(26:30) = binom_rvs(n, p, k)
          n = 10_int8
          p = 0.7
          do i = 31, 35
              res(i) = binom_rvs(n, P)
          end do
          res(36:40) = binom_rvs(n, p, k)
          call check(all(res == ans), &
            msg="binomial_distribution_rvs_int8_int64 failed", warn=warn)
      end subroutine test_binom_rvs_int8

      subroutine test_binom_rvs_int16
          integer(int16) :: res(40), k, n
          integer :: i, m
          real :: p
          integer :: seed, get
          integer(int16) :: ans(40) =                                                 &
                      [71_int16, 81_int16, 75_int16, 78_int16, 74_int16, &
                       77_int16, 81_int16, 80_int16, 81_int16, 78_int16, &
                       10_int16,  5_int16,  6_int16, 12_int16, 10_int16, &
                       10_int16, 10_int16, 11_int16, 11_int16,  9_int16, &
                       28_int16, 27_int16, 26_int16, 28_int16, 19_int16, &
                       20_int16, 29_int16, 25_int16, 23_int16, 27_int16, &
                        6_int16,  4_int16,  5_int16,  5_int16,  7_int16, &
                        8_int16,  6_int16,  7_int16,  8_int16,  7_int16]

          print *, "Test binomial_distribution_rvs_int16"
          seed = 852693417
          call random_seed(seed, get)
          n = 100_int16
          p = 0.76
          k = 5_int16
          do i = 1, 5
            res(i) = binom_rvs(n, p)
          end do
          res(6:10) = binom_rvs(n, p, k)
          n = 20_int16
          p = 0.4
          do i = 11, 15
              res(i) = binom_rvs(n, P)
          end do
          res(16:20) = binom_rvs(n, p, k)

          n = 100_int16
          p = 0.26
          k = 5_int16
          do i = 21, 25
            res(i) = binom_rvs(n, p)
          end do
          res(26:30) = binom_rvs(n, p, k)
          n = 10_int16
          p = 0.7
          do i = 31, 35
              res(i) = binom_rvs(n, P)
          end do
          res(36:40) = binom_rvs(n, p, k)
          call check(all(res == ans), &
            msg="binomial_distribution_rvs_int16_int64 failed", warn=warn)
      end subroutine test_binom_rvs_int16

      subroutine test_binom_rvs_int32
          integer(int32) :: res(40), k, n
          integer :: i, m
          real :: p
          integer :: seed, get
          integer(int32) :: ans(40) =                                                 &
                      [71_int32, 81_int32, 75_int32, 78_int32, 74_int32, &
                       77_int32, 81_int32, 80_int32, 81_int32, 78_int32, &
                       10_int32,  5_int32,  6_int32, 12_int32, 10_int32, &
                       10_int32, 10_int32, 11_int32, 11_int32,  9_int32, &
                       28_int32, 27_int32, 26_int32, 28_int32, 19_int32, &
                       20_int32, 29_int32, 25_int32, 23_int32, 27_int32, &
                        6_int32,  4_int32,  5_int32,  5_int32,  7_int32, &
                        8_int32,  6_int32,  7_int32,  8_int32,  7_int32]

          print *, "Test binomial_distribution_rvs_int32"
          seed = 852693417
          call random_seed(seed, get)
          n = 100_int32
          p = 0.76
          k = 5_int32
          do i = 1, 5
            res(i) = binom_rvs(n, p)
          end do
          res(6:10) = binom_rvs(n, p, k)
          n = 20_int32
          p = 0.4
          do i = 11, 15
              res(i) = binom_rvs(n, P)
          end do
          res(16:20) = binom_rvs(n, p, k)

          n = 100_int32
          p = 0.26
          k = 5_int32
          do i = 21, 25
            res(i) = binom_rvs(n, p)
          end do
          res(26:30) = binom_rvs(n, p, k)
          n = 10_int32
          p = 0.7
          do i = 31, 35
              res(i) = binom_rvs(n, P)
          end do
          res(36:40) = binom_rvs(n, p, k)
          call check(all(res == ans), &
            msg="binomial_distribution_rvs_int32_int64 failed", warn=warn)
      end subroutine test_binom_rvs_int32

      subroutine test_binom_rvs_int64
          integer(int64) :: res(40), k, n
          integer :: i, m
          real :: p
          integer :: seed, get
          integer(int64) :: ans(40) =                                                 &
                      [71_int64, 81_int64, 75_int64, 78_int64, 74_int64, &
                       77_int64, 81_int64, 80_int64, 81_int64, 78_int64, &
                       10_int64,  5_int64,  6_int64, 12_int64, 10_int64, &
                       10_int64, 10_int64, 11_int64, 11_int64,  9_int64, &
                       28_int64, 27_int64, 26_int64, 28_int64, 19_int64, &
                       20_int64, 29_int64, 25_int64, 23_int64, 27_int64, &
                        6_int64,  4_int64,  5_int64,  5_int64,  7_int64, &
                        8_int64,  6_int64,  7_int64,  8_int64,  7_int64]

          print *, "Test binomial_distribution_rvs_int64"
          seed = 852693417
          call random_seed(seed, get)
          n = 100_int64
          p = 0.76
          k = 5_int64
          do i = 1, 5
            res(i) = binom_rvs(n, p)
          end do
          res(6:10) = binom_rvs(n, p, k)
          n = 20_int64
          p = 0.4
          do i = 11, 15
              res(i) = binom_rvs(n, P)
          end do
          res(16:20) = binom_rvs(n, p, k)

          n = 100_int64
          p = 0.26
          k = 5_int64
          do i = 21, 25
            res(i) = binom_rvs(n, p)
          end do
          res(26:30) = binom_rvs(n, p, k)
          n = 10_int64
          p = 0.7
          do i = 31, 35
              res(i) = binom_rvs(n, P)
          end do
          res(36:40) = binom_rvs(n, p, k)
          call check(all(res == ans), &
            msg="binomial_distribution_rvs_int64_int64 failed", warn=warn)
      end subroutine test_binom_rvs_int64


    subroutine test_binom_pmf_int8
        integer(int8) :: x1, x2(3,4),  n
        integer :: i, m
        integer :: seed, get
        real :: p, res(3,5)
        real :: ans(15) = [7.78146312E-02, 7.78146312E-02, 7.78146312E-02, &
                              0.109103285, 3.38223181E-04, 7.78146312E-02, &
                              0.114558451, 1.53707610E-02, 8.07851329E-02, &
                              9.58787426E-02, 6.05889633E-02, 0.114558451, &
                            4.04635631E-02, 5.83609045E-02, 5.83609045E-02]

        print *, "Test binomial_distribution_pmf_int8"
        seed = 630852741
        call random_seed(seed, get)
        n = 50_int8
        p = 0.6
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int8), [3,4])
        res(:,1) = binom_pmf(x1, n, p)
        res(:, 2:5) = binom_pmf(x2, n, p)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="binomial_distribution_pmf_int8 failed", warn=warn)
    end subroutine test_binom_pmf_int8

    subroutine test_binom_pmf_int16
        integer(int16) :: x1, x2(3,4),  n
        integer :: i, m
        integer :: seed, get
        real :: p, res(3,5)
        real :: ans(15) = [7.78146312E-02, 7.78146312E-02, 7.78146312E-02, &
                              0.109103285, 3.38223181E-04, 7.78146312E-02, &
                              0.114558451, 1.53707610E-02, 8.07851329E-02, &
                              9.58787426E-02, 6.05889633E-02, 0.114558451, &
                            4.04635631E-02, 5.83609045E-02, 5.83609045E-02]

        print *, "Test binomial_distribution_pmf_int16"
        seed = 630852741
        call random_seed(seed, get)
        n = 50_int16
        p = 0.6
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int16), [3,4])
        res(:,1) = binom_pmf(x1, n, p)
        res(:, 2:5) = binom_pmf(x2, n, p)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="binomial_distribution_pmf_int16 failed", warn=warn)
    end subroutine test_binom_pmf_int16

    subroutine test_binom_pmf_int32
        integer(int32) :: x1, x2(3,4),  n
        integer :: i, m
        integer :: seed, get
        real :: p, res(3,5)
        real :: ans(15) = [7.78146312E-02, 7.78146312E-02, 7.78146312E-02, &
                              0.109103285, 3.38223181E-04, 7.78146312E-02, &
                              0.114558451, 1.53707610E-02, 8.07851329E-02, &
                              9.58787426E-02, 6.05889633E-02, 0.114558451, &
                            4.04635631E-02, 5.83609045E-02, 5.83609045E-02]

        print *, "Test binomial_distribution_pmf_int32"
        seed = 630852741
        call random_seed(seed, get)
        n = 50_int32
        p = 0.6
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int32), [3,4])
        res(:,1) = binom_pmf(x1, n, p)
        res(:, 2:5) = binom_pmf(x2, n, p)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="binomial_distribution_pmf_int32 failed", warn=warn)
    end subroutine test_binom_pmf_int32

    subroutine test_binom_pmf_int64
        integer(int64) :: x1, x2(3,4),  n
        integer :: i, m
        integer :: seed, get
        real :: p, res(3,5)
        real :: ans(15) = [7.78146312E-02, 7.78146312E-02, 7.78146312E-02, &
                              0.109103285, 3.38223181E-04, 7.78146312E-02, &
                              0.114558451, 1.53707610E-02, 8.07851329E-02, &
                              9.58787426E-02, 6.05889633E-02, 0.114558451, &
                            4.04635631E-02, 5.83609045E-02, 5.83609045E-02]

        print *, "Test binomial_distribution_pmf_int64"
        seed = 630852741
        call random_seed(seed, get)
        n = 50_int64
        p = 0.6
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int64), [3,4])
        res(:,1) = binom_pmf(x1, n, p)
        res(:, 2:5) = binom_pmf(x2, n, p)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="binomial_distribution_pmf_int64 failed", warn=warn)
    end subroutine test_binom_pmf_int64


    subroutine test_binom_cdf_int8
        integer(int8) :: x1, x2(3,4),  n
        integer :: i, m
        integer :: seed, get
        real :: p, res(3,5)
        real :: ans(15) = [0.978971064, 0.978971064, 0.978971064, &
                           0.993534148, 0.872478724, 0.250010669, &
                        5.09519465E-02, 0.125598967, 0.943473637, &
                           0.872478724, 0.978971064, 0.595598698, &
                           0.250010669, 0.125598967, 0.872478724]

        print *, "Test binomial_distribution_cdf_int8"
        seed = 17428396
        call random_seed(seed, get)
        n = 20_int8
        p = 0.4
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int8), [3,4])
        res(:,1) = binom_cdf(x1, n, p)
        res(:, 2:5) = binom_cdf(x2, n, p)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="binomial_distribution_cdf_int8 failed", warn=warn)
    end subroutine test_binom_cdf_int8

    subroutine test_binom_cdf_int16
        integer(int16) :: x1, x2(3,4),  n
        integer :: i, m
        integer :: seed, get
        real :: p, res(3,5)
        real :: ans(15) = [0.978971064, 0.978971064, 0.978971064, &
                           0.993534148, 0.872478724, 0.250010669, &
                        5.09519465E-02, 0.125598967, 0.943473637, &
                           0.872478724, 0.978971064, 0.595598698, &
                           0.250010669, 0.125598967, 0.872478724]

        print *, "Test binomial_distribution_cdf_int16"
        seed = 17428396
        call random_seed(seed, get)
        n = 20_int16
        p = 0.4
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int16), [3,4])
        res(:,1) = binom_cdf(x1, n, p)
        res(:, 2:5) = binom_cdf(x2, n, p)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="binomial_distribution_cdf_int16 failed", warn=warn)
    end subroutine test_binom_cdf_int16

    subroutine test_binom_cdf_int32
        integer(int32) :: x1, x2(3,4),  n
        integer :: i, m
        integer :: seed, get
        real :: p, res(3,5)
        real :: ans(15) = [0.978971064, 0.978971064, 0.978971064, &
                           0.993534148, 0.872478724, 0.250010669, &
                        5.09519465E-02, 0.125598967, 0.943473637, &
                           0.872478724, 0.978971064, 0.595598698, &
                           0.250010669, 0.125598967, 0.872478724]

        print *, "Test binomial_distribution_cdf_int32"
        seed = 17428396
        call random_seed(seed, get)
        n = 20_int32
        p = 0.4
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int32), [3,4])
        res(:,1) = binom_cdf(x1, n, p)
        res(:, 2:5) = binom_cdf(x2, n, p)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="binomial_distribution_cdf_int32 failed", warn=warn)
    end subroutine test_binom_cdf_int32

    subroutine test_binom_cdf_int64
        integer(int64) :: x1, x2(3,4),  n
        integer :: i, m
        integer :: seed, get
        real :: p, res(3,5)
        real :: ans(15) = [0.978971064, 0.978971064, 0.978971064, &
                           0.993534148, 0.872478724, 0.250010669, &
                        5.09519465E-02, 0.125598967, 0.943473637, &
                           0.872478724, 0.978971064, 0.595598698, &
                           0.250010669, 0.125598967, 0.872478724]

        print *, "Test binomial_distribution_cdf_int64"
        seed = 17428396
        call random_seed(seed, get)
        n = 20_int64
        p = 0.4
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int64), [3,4])
        res(:,1) = binom_cdf(x1, n, p)
        res(:, 2:5) = binom_cdf(x2, n, p)
        call check(all(abs(res - reshape(ans, [3,5])) < sptol), &
            msg="binomial_distribution_cdf_int64 failed", warn=warn)
    end subroutine test_binom_cdf_int64


end program test_distribution