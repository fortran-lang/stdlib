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

    logical ::  warn = .true.

    call test_random_seed
    call test_uniform_random
    call test_normal_random
    call test_binomial_random

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
        integer(int32) :: ans(5) = [-1877935254, 662282448, -99504370, &
                                    -1700386229, -749448883]
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

    subroutine test_uniform_random
        integer :: i, j, freq(0:999), num=10000000
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
        write(*,*) "Std. Dev. of chi-squared with 999 d. of f. = 44.70"
        write(*,*) "Values of chi-squared should be within 90. of 999."
        write(*,*) "Chi-squared for uniform random generator is : ", chisq
        call check(abs(999-chisq) < 90.0, &
               msg="uniform randomness failed chi-squared test", warn=warn)
    end subroutine test_uniform_random

    subroutine test_normal_random
        integer :: i, j, freq(0:999), num=10000000
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
        write(*,*) "Std. Dev. of chi-squared with 999 d. of f. = 44.70"
        write(*,*) "Values of chi-squared should be within 90. of 999."
        write(*,*) "Chi-squared for normal random generator is : ", chisq
        call check(abs(999-chisq) < 90.0, &
               msg="normal randomness failed chi-squared test", warn=warn)
    end subroutine test_normal_random

    subroutine test_binomial_random
        integer :: i, j, n, num=10000000
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
           expct = binom_pmf(i, n, p)
           chisq = chisq + (freq(i)/real(num, kind=dp) - expct) ** 2 / expct
        end do
        write(*,*) "chi-squared with 99.9% Confidence (9 d. of f) is 27.88"
        write(*,*) "Values of chi-squared should be less than 27.88"
        write(*,*) "Chi-squared for binomial random generator is : ", chisq
        call check(abs(chisq) < 27.88, &
             msg="binomial1 randomness failed chi-squared test", warn=warn)
        n = 50
        p = 0.4
        deallocate(freq)
        allocate(freq(0:n))
        freq = 0
        do i = 1, num
           j = binom_rvs(n, p)
           freq(j) = freq(j) + 1
        end do
        chisq = 0.0_dp
        do i = 0, n
           expct = binom_pmf(i, n, p)
           chisq = chisq + (freq(i)/real(num, kind=dp) - expct) ** 2 / expct
        end do
        write(*,*) "chi-squared with 99.9% Confidence (39 d. of f) is 72.05"
        write(*,*) "Values of chi-squared should be less than 72.05"
        write(*,*) "Chi-squared for binomial random generator is : ", chisq
        call check(abs(chisq) < 27.88, &
             msg="binomial1 randomness failed chi-squared test", warn=warn)
    end subroutine test_binomial_random

      subroutine test_uni_rvs_sp_int8
          real(sp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(sp) :: ans(20) =[5.04399538E-02_sp, 0.404976368_sp, &
                                    0.541224837_sp,    0.444294989_sp, &
                                   2.11626887E-02_sp,  0.675580144_sp, &
                                    0.271609545_sp,    0.195743740_sp, &
                                    0.818225145_sp,    0.317105770_sp, &
                                   7.34233856E-02_sp,  0.938642502_sp, &
                                    -0.928170681_sp,  -0.290764689_sp, &
                                    0.924183130_sp,   -0.428336263_sp, &
                                   -2.76409388E-02_sp, -0.851551056_sp,&
                                    -0.131302118_sp,   -0.472380519_sp]

          print *, "Test uniform_distribution_rvs_sp_int8"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_sp_int8 failed", warn=warn)
      end subroutine test_uni_rvs_sp_int8

      subroutine test_uni_rvs_sp_int16
          real(sp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(sp) :: ans(20) =[5.04399538E-02_sp, 0.404976368_sp, &
                                    0.541224837_sp,    0.444294989_sp, &
                                   2.11626887E-02_sp,  0.675580144_sp, &
                                    0.271609545_sp,    0.195743740_sp, &
                                    0.818225145_sp,    0.317105770_sp, &
                                   7.34233856E-02_sp,  0.938642502_sp, &
                                    -0.928170681_sp,  -0.290764689_sp, &
                                    0.924183130_sp,   -0.428336263_sp, &
                                   -2.76409388E-02_sp, -0.851551056_sp,&
                                    -0.131302118_sp,   -0.472380519_sp]

          print *, "Test uniform_distribution_rvs_sp_int16"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_sp_int16 failed", warn=warn)
      end subroutine test_uni_rvs_sp_int16

      subroutine test_uni_rvs_sp_int32
          real(sp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(sp) :: ans(20) =[5.04399538E-02_sp, 0.404976368_sp, &
                                    0.541224837_sp,    0.444294989_sp, &
                                   2.11626887E-02_sp,  0.675580144_sp, &
                                    0.271609545_sp,    0.195743740_sp, &
                                    0.818225145_sp,    0.317105770_sp, &
                                   7.34233856E-02_sp,  0.938642502_sp, &
                                    -0.928170681_sp,  -0.290764689_sp, &
                                    0.924183130_sp,   -0.428336263_sp, &
                                   -2.76409388E-02_sp, -0.851551056_sp,&
                                    -0.131302118_sp,   -0.472380519_sp]

          print *, "Test uniform_distribution_rvs_sp_int32"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_sp_int32 failed", warn=warn)
      end subroutine test_uni_rvs_sp_int32

      subroutine test_uni_rvs_sp_int64
          real(sp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(sp) :: ans(20) =[5.04399538E-02_sp, 0.404976368_sp, &
                                    0.541224837_sp,    0.444294989_sp, &
                                   2.11626887E-02_sp,  0.675580144_sp, &
                                    0.271609545_sp,    0.195743740_sp, &
                                    0.818225145_sp,    0.317105770_sp, &
                                   7.34233856E-02_sp,  0.938642502_sp, &
                                    -0.928170681_sp,  -0.290764689_sp, &
                                    0.924183130_sp,   -0.428336263_sp, &
                                   -2.76409388E-02_sp, -0.851551056_sp,&
                                    -0.131302118_sp,   -0.472380519_sp]

          print *, "Test uniform_distribution_rvs_sp_int64"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_sp_int64 failed", warn=warn)
      end subroutine test_uni_rvs_sp_int64

      subroutine test_uni_rvs_dp_int8
          real(dp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(dp) :: ans(20) =                                         &
          [5.0440000879492075E-002_dp,  0.40497642331439698_dp,     &
           0.54122488655003531_dp,      0.44429503556832806_dp,     &
           2.1162694927125303E-002_dp,  0.67558018012260379_dp,     &
           0.27160956576539419_dp,      0.19574374714389442_dp,     &
           0.81822516564901226_dp,      0.31710580272090028_dp,     &
           7.3423453979779563E-002_dp,  0.93864258396598932_dp,     &
           -0.92817065158966483_dp,     -0.29076465556855546_dp,    &
           0.92418322617268323_dp,      -0.42833614595047353_dp,    &
           -2.7640916362106749E-002_dp, -0.85155101090234475_dp,    &
            -0.13130202969796589_dp,     -0.47238048619523831_dp]

          print *, "Test uniform_distribution_rvs_dp_int8"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_dp_int8 failed", warn=warn)
      end subroutine test_uni_rvs_dp_int8

      subroutine test_uni_rvs_dp_int16
          real(dp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(dp) :: ans(20) =                                         &
          [5.0440000879492075E-002_dp,  0.40497642331439698_dp,     &
           0.54122488655003531_dp,      0.44429503556832806_dp,     &
           2.1162694927125303E-002_dp,  0.67558018012260379_dp,     &
           0.27160956576539419_dp,      0.19574374714389442_dp,     &
           0.81822516564901226_dp,      0.31710580272090028_dp,     &
           7.3423453979779563E-002_dp,  0.93864258396598932_dp,     &
           -0.92817065158966483_dp,     -0.29076465556855546_dp,    &
           0.92418322617268323_dp,      -0.42833614595047353_dp,    &
           -2.7640916362106749E-002_dp, -0.85155101090234475_dp,    &
            -0.13130202969796589_dp,     -0.47238048619523831_dp]

          print *, "Test uniform_distribution_rvs_dp_int16"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_dp_int16 failed", warn=warn)
      end subroutine test_uni_rvs_dp_int16

      subroutine test_uni_rvs_dp_int32
          real(dp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(dp) :: ans(20) =                                         &
          [5.0440000879492075E-002_dp,  0.40497642331439698_dp,     &
           0.54122488655003531_dp,      0.44429503556832806_dp,     &
           2.1162694927125303E-002_dp,  0.67558018012260379_dp,     &
           0.27160956576539419_dp,      0.19574374714389442_dp,     &
           0.81822516564901226_dp,      0.31710580272090028_dp,     &
           7.3423453979779563E-002_dp,  0.93864258396598932_dp,     &
           -0.92817065158966483_dp,     -0.29076465556855546_dp,    &
           0.92418322617268323_dp,      -0.42833614595047353_dp,    &
           -2.7640916362106749E-002_dp, -0.85155101090234475_dp,    &
            -0.13130202969796589_dp,     -0.47238048619523831_dp]

          print *, "Test uniform_distribution_rvs_dp_int32"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_dp_int32 failed", warn=warn)
      end subroutine test_uni_rvs_dp_int32

      subroutine test_uni_rvs_dp_int64
          real(dp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(dp) :: ans(20) =                                         &
          [5.0440000879492075E-002_dp,  0.40497642331439698_dp,     &
           0.54122488655003531_dp,      0.44429503556832806_dp,     &
           2.1162694927125303E-002_dp,  0.67558018012260379_dp,     &
           0.27160956576539419_dp,      0.19574374714389442_dp,     &
           0.81822516564901226_dp,      0.31710580272090028_dp,     &
           7.3423453979779563E-002_dp,  0.93864258396598932_dp,     &
           -0.92817065158966483_dp,     -0.29076465556855546_dp,    &
           0.92418322617268323_dp,      -0.42833614595047353_dp,    &
           -2.7640916362106749E-002_dp, -0.85155101090234475_dp,    &
            -0.13130202969796589_dp,     -0.47238048619523831_dp]

          print *, "Test uniform_distribution_rvs_dp_int64"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_dp_int64 failed", warn=warn)
      end subroutine test_uni_rvs_dp_int64

      subroutine test_uni_rvs_qp_int8
          real(qp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(qp) :: ans(20) =                                            &
                          [5.04400008794921155227701397235054593E-0002_qp, &
                          0.541224886550035344905351351633815116_qp,       &
                          2.11626949271253231253697030062588044E-0002_qp,  &
                          0.271609565765394294354172555376337108_qp,       &
                          0.818225165649012287664483469044586672_qp,       &
                          0.536711726989889872841273961390337156_qp,       &
                          3.59146742051676567559558750654760490E-0002_qp,  &
                          0.962091613086341655371670502707455153_qp,       &
                          0.486179541818946734433314608045830654_qp,       &
                          0.434348985151017063548868025509018790_qp,       &
                          0.106281673372014829665969814869378804_qp,       &
                          0.993851991956529253996325479550140463_qp,       &
                          -3.90848050485929016806725507936877048E-0002_qp, &
                          -0.858878895086928935915040685801935332_qp,      &
                          0.251031308226342431137236520529691044_qp,       &
                          0.471810997198912336599631922994337833_qp,       &
                          -0.407856981586257069522082601191682443_qp,      &
                          -0.311821151597713862561429022059299144_qp,      &
                          -0.758632615531082761324702937561604109_qp,      &
                          4.95379602455143175791880174687944969E-0002_qp]

          print *, "Test uniform_distribution_rvs_qp_int8"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_qp_int8 failed", warn=warn)
      end subroutine test_uni_rvs_qp_int8

      subroutine test_uni_rvs_qp_int16
          real(qp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(qp) :: ans(20) =                                            &
                          [5.04400008794921155227701397235054593E-0002_qp, &
                          0.541224886550035344905351351633815116_qp,       &
                          2.11626949271253231253697030062588044E-0002_qp,  &
                          0.271609565765394294354172555376337108_qp,       &
                          0.818225165649012287664483469044586672_qp,       &
                          0.536711726989889872841273961390337156_qp,       &
                          3.59146742051676567559558750654760490E-0002_qp,  &
                          0.962091613086341655371670502707455153_qp,       &
                          0.486179541818946734433314608045830654_qp,       &
                          0.434348985151017063548868025509018790_qp,       &
                          0.106281673372014829665969814869378804_qp,       &
                          0.993851991956529253996325479550140463_qp,       &
                          -3.90848050485929016806725507936877048E-0002_qp, &
                          -0.858878895086928935915040685801935332_qp,      &
                          0.251031308226342431137236520529691044_qp,       &
                          0.471810997198912336599631922994337833_qp,       &
                          -0.407856981586257069522082601191682443_qp,      &
                          -0.311821151597713862561429022059299144_qp,      &
                          -0.758632615531082761324702937561604109_qp,      &
                          4.95379602455143175791880174687944969E-0002_qp]

          print *, "Test uniform_distribution_rvs_qp_int16"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_qp_int16 failed", warn=warn)
      end subroutine test_uni_rvs_qp_int16

      subroutine test_uni_rvs_qp_int32
          real(qp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(qp) :: ans(20) =                                            &
                          [5.04400008794921155227701397235054593E-0002_qp, &
                          0.541224886550035344905351351633815116_qp,       &
                          2.11626949271253231253697030062588044E-0002_qp,  &
                          0.271609565765394294354172555376337108_qp,       &
                          0.818225165649012287664483469044586672_qp,       &
                          0.536711726989889872841273961390337156_qp,       &
                          3.59146742051676567559558750654760490E-0002_qp,  &
                          0.962091613086341655371670502707455153_qp,       &
                          0.486179541818946734433314608045830654_qp,       &
                          0.434348985151017063548868025509018790_qp,       &
                          0.106281673372014829665969814869378804_qp,       &
                          0.993851991956529253996325479550140463_qp,       &
                          -3.90848050485929016806725507936877048E-0002_qp, &
                          -0.858878895086928935915040685801935332_qp,      &
                          0.251031308226342431137236520529691044_qp,       &
                          0.471810997198912336599631922994337833_qp,       &
                          -0.407856981586257069522082601191682443_qp,      &
                          -0.311821151597713862561429022059299144_qp,      &
                          -0.758632615531082761324702937561604109_qp,      &
                          4.95379602455143175791880174687944969E-0002_qp]

          print *, "Test uniform_distribution_rvs_qp_int32"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_qp_int32 failed", warn=warn)
      end subroutine test_uni_rvs_qp_int32

      subroutine test_uni_rvs_qp_int64
          real(qp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(qp) :: ans(20) =                                            &
                          [5.04400008794921155227701397235054593E-0002_qp, &
                          0.541224886550035344905351351633815116_qp,       &
                          2.11626949271253231253697030062588044E-0002_qp,  &
                          0.271609565765394294354172555376337108_qp,       &
                          0.818225165649012287664483469044586672_qp,       &
                          0.536711726989889872841273961390337156_qp,       &
                          3.59146742051676567559558750654760490E-0002_qp,  &
                          0.962091613086341655371670502707455153_qp,       &
                          0.486179541818946734433314608045830654_qp,       &
                          0.434348985151017063548868025509018790_qp,       &
                          0.106281673372014829665969814869378804_qp,       &
                          0.993851991956529253996325479550140463_qp,       &
                          -3.90848050485929016806725507936877048E-0002_qp, &
                          -0.858878895086928935915040685801935332_qp,      &
                          0.251031308226342431137236520529691044_qp,       &
                          0.471810997198912336599631922994337833_qp,       &
                          -0.407856981586257069522082601191682443_qp,      &
                          -0.311821151597713862561429022059299144_qp,      &
                          -0.758632615531082761324702937561604109_qp,      &
                          4.95379602455143175791880174687944969E-0002_qp]

          print *, "Test uniform_distribution_rvs_qp_int64"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 246813579
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="uniform_distribution_rvs_qp_int64 failed", warn=warn)
      end subroutine test_uni_rvs_qp_int64


    subroutine test_uni_pdf_sp
        real(sp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(sp) :: ans(15) = [(0.2_sp, i=1,15)]

        print *, "Test uniform_distribution_pdf_sp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 147258
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = 0.0_sp
        scale = 5.0_sp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(res == reshape(ans,[3,5])), &
            msg="uniform_distribution_pdf_sp failed", warn=warn)
    end subroutine test_uni_pdf_sp

    subroutine test_uni_pdf_dp
        real(dp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(dp) :: ans(15) = [(0.2_dp, i=1,15)]

        print *, "Test uniform_distribution_pdf_dp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 147258
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = 0.0_dp
        scale = 5.0_dp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(res == reshape(ans,[3,5])), &
            msg="uniform_distribution_pdf_dp failed", warn=warn)
    end subroutine test_uni_pdf_dp

    subroutine test_uni_pdf_qp
        real(qp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(qp) :: ans(15) = [(0.2_qp, i=1,15)]

        print *, "Test uniform_distribution_pdf_qp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 147258
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = 0.0_qp
        scale = 5.0_qp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_pdf(x1, loc, scale)
        res(:, 2:5) = uni_pdf(x2, loc, scale)
        call check(all(res == reshape(ans,[3,5])), &
            msg="uniform_distribution_pdf_qp failed", warn=warn)
    end subroutine test_uni_pdf_qp


    subroutine test_uni_cdf_sp
        real(sp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(sp) :: ans(15) =                                              &
                    [0.370952129_sp,0.370952129_sp,0.370952129_sp, &
                     0.211502790_sp,0.856203020_sp,0.249337375_sp, &
                     0.591968060_sp,0.740594268_sp,0.916763842_sp, &
                     0.489238262_sp,0.669350743_sp,0.325252831_sp, &
                     0.709118247_sp,0.993179202_sp, 0.533772647_sp]

        print *, "Test uniform_distribution_cdf_sp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 369147
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = -1.0_sp
        scale = 2.0_sp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(res == reshape(ans,[3,5])), &
            msg="uniform_distribution_cdf_sp failed", warn=warn)
    end subroutine test_uni_cdf_sp

    subroutine test_uni_cdf_dp
        real(dp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(dp) :: ans(15) =                                              &
                    [0.37095218073698477_dp, 0.37095218073698477_dp,   &
                     0.37095218073698477_dp, 0.21150283077637966_dp,   &
                     0.85620302201687337_dp, 0.24933739254107234_dp,   &
                     0.59196810028349733_dp, 0.74059427348487261_dp,   &
                     0.91676389409365489_dp, 0.48923829899816573_dp,   &
                     0.66935075798058086_dp, 0.32525285602311815_dp,   &
                     0.70911828027725143_dp, 0.99317925146050590_dp,   &
                     0.53377268162347080_dp]

        print *, "Test uniform_distribution_cdf_dp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 369147
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = -1.0_dp
        scale = 2.0_dp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(res == reshape(ans,[3,5])), &
            msg="uniform_distribution_cdf_dp failed", warn=warn)
    end subroutine test_uni_cdf_dp

    subroutine test_uni_cdf_qp
        real(qp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(qp) :: ans(15) =                                              &
                         [0.370952180736984871103049706441149152_qp,       &
                          0.370952180736984871103049706441149152_qp,       &
                          0.370952180736984871103049706441149152_qp,       &
                          0.856203022016873431437082922335257188_qp,       &
                          0.591968100283497363104766952623951312_qp,       &
                          0.916763894093654990339098230587599785_qp,       &
                          0.669350757980580953026013543272769513_qp,       &
                          0.709118280277251507856777024849348465_qp,       &
                          0.533772681623470893479628891925566616_qp,       &
                          0.184652325598168172862168566862529286_qp,       &
                          0.126339697734662318442576788018635888_qp,       &
                          0.371498517813914695782230968594334580_qp,       &
                          0.798823345448597328078421235329170698_qp,       &
                          2.62105197219545345857038388258213763E-0002_qp,  &
                          0.691944196267230851290235330359880028_qp]

        print *, "Test uniform_distribution_cdf_qp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 369147
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = -1.0_qp
        scale = 2.0_qp
        x1 = uni_rvs(loc, scale)
        x2 = reshape(uni_rvs(loc, scale, 12), [3,4])
        res(:,1) = uni_cdf(x1, loc, scale)
        res(:, 2:5) = uni_cdf(x2, loc, scale)
        call check(all(res == reshape(ans,[3,5])), &
            msg="uniform_distribution_cdf_qp failed", warn=warn)
    end subroutine test_uni_cdf_qp


      subroutine test_nor_rvs_sp_int8
          real(sp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(sp) :: ans(20) =                                            &
                [0.614553273_sp, -0.361076236_sp, -0.577031732_sp, &
             -0.586278856_sp,7.04725906E-02_sp,-1.69806127E-02_sp, &
                -1.74709845_sp,  -0.442299634_sp,   1.02155888_sp, &
               -0.741506457_sp, -0.198480308_sp,  -0.622205317_sp, &
                  -2.29946089_sp,  1.34173250_sp,  0.231525183_sp, &
                  -1.29622912_sp, -0.973707318_sp, -2.53301716_sp, &
                  -3.36481953_sp,  -0.488066018_sp]

          print *, "Test normal_distribution_rvs_sp_int8"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_sp_int8 failed", warn=warn)
      end subroutine test_nor_rvs_sp_int8

      subroutine test_nor_rvs_sp_int16
          real(sp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(sp) :: ans(20) =                                            &
                [0.614553273_sp, -0.361076236_sp, -0.577031732_sp, &
             -0.586278856_sp,7.04725906E-02_sp,-1.69806127E-02_sp, &
                -1.74709845_sp,  -0.442299634_sp,   1.02155888_sp, &
               -0.741506457_sp, -0.198480308_sp,  -0.622205317_sp, &
                  -2.29946089_sp,  1.34173250_sp,  0.231525183_sp, &
                  -1.29622912_sp, -0.973707318_sp, -2.53301716_sp, &
                  -3.36481953_sp,  -0.488066018_sp]

          print *, "Test normal_distribution_rvs_sp_int16"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_sp_int16 failed", warn=warn)
      end subroutine test_nor_rvs_sp_int16

      subroutine test_nor_rvs_sp_int32
          real(sp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(sp) :: ans(20) =                                            &
                [0.614553273_sp, -0.361076236_sp, -0.577031732_sp, &
             -0.586278856_sp,7.04725906E-02_sp,-1.69806127E-02_sp, &
                -1.74709845_sp,  -0.442299634_sp,   1.02155888_sp, &
               -0.741506457_sp, -0.198480308_sp,  -0.622205317_sp, &
                  -2.29946089_sp,  1.34173250_sp,  0.231525183_sp, &
                  -1.29622912_sp, -0.973707318_sp, -2.53301716_sp, &
                  -3.36481953_sp,  -0.488066018_sp]

          print *, "Test normal_distribution_rvs_sp_int32"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_sp_int32 failed", warn=warn)
      end subroutine test_nor_rvs_sp_int32

      subroutine test_nor_rvs_sp_int64
          real(sp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(sp) :: ans(20) =                                            &
                [0.614553273_sp, -0.361076236_sp, -0.577031732_sp, &
             -0.586278856_sp,7.04725906E-02_sp,-1.69806127E-02_sp, &
                -1.74709845_sp,  -0.442299634_sp,   1.02155888_sp, &
               -0.741506457_sp, -0.198480308_sp,  -0.622205317_sp, &
                  -2.29946089_sp,  1.34173250_sp,  0.231525183_sp, &
                  -1.29622912_sp, -0.973707318_sp, -2.53301716_sp, &
                  -3.36481953_sp,  -0.488066018_sp]

          print *, "Test normal_distribution_rvs_sp_int64"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_sp_int64 failed", warn=warn)
      end subroutine test_nor_rvs_sp_int64

      subroutine test_nor_rvs_dp_int8
          real(dp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(dp) :: ans(20) =                                            &
                     [0.61455325290729768_dp, -0.36107623852785842_dp, &
                     -0.57703174081498632_dp, -0.58627888088625235_dp, &
              7.0472593998527538E-002_dp, -1.6980611954831810E-002_dp, &
                      -1.7470984096901425_dp, -0.44229963147825985_dp, &
                      1.0215589011314947_dp,  -0.74150647352232535_dp, &
                     -0.19848027921436251_dp, -0.62220532873876910_dp, &
                       -2.2994609294752779_dp,  1.3417324345158637_dp, &
                      0.23152514867199403_dp,  -1.2962291146722167_dp, &
                      -0.97370732713792285_dp, -2.5330172536546929_dp, &
                      -3.3648195565744432_dp,  -0.48806599977557108_dp]

          print *, "Test normal_distribution_rvs_dp_int8"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_dp_int8 failed", warn=warn)
      end subroutine test_nor_rvs_dp_int8

      subroutine test_nor_rvs_dp_int16
          real(dp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(dp) :: ans(20) =                                            &
                     [0.61455325290729768_dp, -0.36107623852785842_dp, &
                     -0.57703174081498632_dp, -0.58627888088625235_dp, &
              7.0472593998527538E-002_dp, -1.6980611954831810E-002_dp, &
                      -1.7470984096901425_dp, -0.44229963147825985_dp, &
                      1.0215589011314947_dp,  -0.74150647352232535_dp, &
                     -0.19848027921436251_dp, -0.62220532873876910_dp, &
                       -2.2994609294752779_dp,  1.3417324345158637_dp, &
                      0.23152514867199403_dp,  -1.2962291146722167_dp, &
                      -0.97370732713792285_dp, -2.5330172536546929_dp, &
                      -3.3648195565744432_dp,  -0.48806599977557108_dp]

          print *, "Test normal_distribution_rvs_dp_int16"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_dp_int16 failed", warn=warn)
      end subroutine test_nor_rvs_dp_int16

      subroutine test_nor_rvs_dp_int32
          real(dp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(dp) :: ans(20) =                                            &
                     [0.61455325290729768_dp, -0.36107623852785842_dp, &
                     -0.57703174081498632_dp, -0.58627888088625235_dp, &
              7.0472593998527538E-002_dp, -1.6980611954831810E-002_dp, &
                      -1.7470984096901425_dp, -0.44229963147825985_dp, &
                      1.0215589011314947_dp,  -0.74150647352232535_dp, &
                     -0.19848027921436251_dp, -0.62220532873876910_dp, &
                       -2.2994609294752779_dp,  1.3417324345158637_dp, &
                      0.23152514867199403_dp,  -1.2962291146722167_dp, &
                      -0.97370732713792285_dp, -2.5330172536546929_dp, &
                      -3.3648195565744432_dp,  -0.48806599977557108_dp]

          print *, "Test normal_distribution_rvs_dp_int32"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_dp_int32 failed", warn=warn)
      end subroutine test_nor_rvs_dp_int32

      subroutine test_nor_rvs_dp_int64
          real(dp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(dp) :: ans(20) =                                            &
                     [0.61455325290729768_dp, -0.36107623852785842_dp, &
                     -0.57703174081498632_dp, -0.58627888088625235_dp, &
              7.0472593998527538E-002_dp, -1.6980611954831810E-002_dp, &
                      -1.7470984096901425_dp, -0.44229963147825985_dp, &
                      1.0215589011314947_dp,  -0.74150647352232535_dp, &
                     -0.19848027921436251_dp, -0.62220532873876910_dp, &
                       -2.2994609294752779_dp,  1.3417324345158637_dp, &
                      0.23152514867199403_dp,  -1.2962291146722167_dp, &
                      -0.97370732713792285_dp, -2.5330172536546929_dp, &
                      -3.3648195565744432_dp,  -0.48806599977557108_dp]

          print *, "Test normal_distribution_rvs_dp_int64"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_dp_int64 failed", warn=warn)
      end subroutine test_nor_rvs_dp_int64

      subroutine test_nor_rvs_qp_int8
          real(qp) :: res(20), loc, scale
          integer(int8) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(qp) :: ans(20) =                                            &
                               [0.614553252907297675733389041852205992_qp, &
                               -0.361076238527858417448612726730061695_qp, &
                               -0.577031740814986315868395649886224419_qp, &
                               -0.586278880886252351523069137329002842_qp, &
                           7.04725939985275384724872083097579889E-0002_qp, &
                          -1.69806119548318104617301571579446318E-0002_qp, &
                                -1.74709840969014251754742872435599566_qp, &
                               -0.442299631478259847039424812464858405_qp, &
                                 1.02155890113149472320230870536761358_qp, &
                               -0.741506473522325348923800447664689273_qp, &
                               -0.198480279214362509421221147931646556_qp, &
                               -0.622205328738769103402717064454918727_qp, &
                                -2.29946092947527813699082344101043418_qp, &
                                 1.34173243451586365893035690532997251_qp, &
                                0.231525148671994029925258473667781800_qp, &
                                -1.29622911467221674230998473831277806_qp, &
                               -0.973707327137922892812404995765973581_qp, &
                                -2.53301725365469310169430627865949646_qp, &
                                -3.36481955657444320451077146572060883_qp, &
                                -0.488065999775571079943858876504236832_qp]

          print *, "Test normal_distribution_rvs_qp_int8"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_qp_int8 failed", warn=warn)
      end subroutine test_nor_rvs_qp_int8

      subroutine test_nor_rvs_qp_int16
          real(qp) :: res(20), loc, scale
          integer(int16) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(qp) :: ans(20) =                                            &
                               [0.614553252907297675733389041852205992_qp, &
                               -0.361076238527858417448612726730061695_qp, &
                               -0.577031740814986315868395649886224419_qp, &
                               -0.586278880886252351523069137329002842_qp, &
                           7.04725939985275384724872083097579889E-0002_qp, &
                          -1.69806119548318104617301571579446318E-0002_qp, &
                                -1.74709840969014251754742872435599566_qp, &
                               -0.442299631478259847039424812464858405_qp, &
                                 1.02155890113149472320230870536761358_qp, &
                               -0.741506473522325348923800447664689273_qp, &
                               -0.198480279214362509421221147931646556_qp, &
                               -0.622205328738769103402717064454918727_qp, &
                                -2.29946092947527813699082344101043418_qp, &
                                 1.34173243451586365893035690532997251_qp, &
                                0.231525148671994029925258473667781800_qp, &
                                -1.29622911467221674230998473831277806_qp, &
                               -0.973707327137922892812404995765973581_qp, &
                                -2.53301725365469310169430627865949646_qp, &
                                -3.36481955657444320451077146572060883_qp, &
                                -0.488065999775571079943858876504236832_qp]

          print *, "Test normal_distribution_rvs_qp_int16"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_qp_int16 failed", warn=warn)
      end subroutine test_nor_rvs_qp_int16

      subroutine test_nor_rvs_qp_int32
          real(qp) :: res(20), loc, scale
          integer(int32) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(qp) :: ans(20) =                                            &
                               [0.614553252907297675733389041852205992_qp, &
                               -0.361076238527858417448612726730061695_qp, &
                               -0.577031740814986315868395649886224419_qp, &
                               -0.586278880886252351523069137329002842_qp, &
                           7.04725939985275384724872083097579889E-0002_qp, &
                          -1.69806119548318104617301571579446318E-0002_qp, &
                                -1.74709840969014251754742872435599566_qp, &
                               -0.442299631478259847039424812464858405_qp, &
                                 1.02155890113149472320230870536761358_qp, &
                               -0.741506473522325348923800447664689273_qp, &
                               -0.198480279214362509421221147931646556_qp, &
                               -0.622205328738769103402717064454918727_qp, &
                                -2.29946092947527813699082344101043418_qp, &
                                 1.34173243451586365893035690532997251_qp, &
                                0.231525148671994029925258473667781800_qp, &
                                -1.29622911467221674230998473831277806_qp, &
                               -0.973707327137922892812404995765973581_qp, &
                                -2.53301725365469310169430627865949646_qp, &
                                -3.36481955657444320451077146572060883_qp, &
                                -0.488065999775571079943858876504236832_qp]

          print *, "Test normal_distribution_rvs_qp_int32"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_qp_int32 failed", warn=warn)
      end subroutine test_nor_rvs_qp_int32

      subroutine test_nor_rvs_qp_int64
          real(qp) :: res(20), loc, scale
          integer(int64) :: k
          integer :: i, n
          integer, allocatable :: seed(:)
          real(qp) :: ans(20) =                                            &
                               [0.614553252907297675733389041852205992_qp, &
                               -0.361076238527858417448612726730061695_qp, &
                               -0.577031740814986315868395649886224419_qp, &
                               -0.586278880886252351523069137329002842_qp, &
                           7.04725939985275384724872083097579889E-0002_qp, &
                          -1.69806119548318104617301571579446318E-0002_qp, &
                                -1.74709840969014251754742872435599566_qp, &
                               -0.442299631478259847039424812464858405_qp, &
                                 1.02155890113149472320230870536761358_qp, &
                               -0.741506473522325348923800447664689273_qp, &
                               -0.198480279214362509421221147931646556_qp, &
                               -0.622205328738769103402717064454918727_qp, &
                                -2.29946092947527813699082344101043418_qp, &
                                 1.34173243451586365893035690532997251_qp, &
                                0.231525148671994029925258473667781800_qp, &
                                -1.29622911467221674230998473831277806_qp, &
                               -0.973707327137922892812404995765973581_qp, &
                                -2.53301725365469310169430627865949646_qp, &
                                -3.36481955657444320451077146572060883_qp, &
                                -0.488065999775571079943858876504236832_qp]

          print *, "Test normal_distribution_rvs_qp_int64"
          call random_seed(size=n)
          allocate(seed(n))
          seed(1) = 25836914
          do i = 2, n
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          call check(all(res == ans), &
            msg="normal_distribution_rvs_qp_int64 failed", warn=warn)
      end subroutine test_nor_rvs_qp_int64



    subroutine test_nor_pdf_sp
        real(sp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(sp) :: ans(15) =                                              &
                  [0.318893105_sp, 0.318893105_sp, 0.318893105_sp, &
                   0.187098116_sp, 0.381644130_sp, 0.284406245_sp, &
                3.34430858E-02_sp, 0.152758196_sp, 0.379699051_sp, &
             4.55921367E-02_sp, 0.397922993_sp, 9.22310278E-02_sp, &
                3.07115261E-02_sp, 0.130263299_sp, 0.393456221_sp]

        print *, "Test normal_distribution_pdf_sp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 741852963
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = 0.0_sp
        scale = 1.0_sp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(res == reshape(ans, [3,5])), &
            msg="normal_distribution_pdf_sp failed", warn=warn)
    end subroutine test_nor_pdf_sp

    subroutine test_nor_pdf_dp
        real(dp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(dp) :: ans(15) =                                              &
                      [0.31889311231886958_dp, 0.31889311231886958_dp, &
                       0.31889311231886958_dp, 0.18709811395929898_dp, &
                       0.38164412797984193_dp, 0.28440622612587790_dp, &
                   3.3443077914987011E-002_dp, 0.15275819526819515_dp, &
                   0.37969903205538591_dp, 4.5592133624642026E-002_dp, &
                   0.39792300977853962_dp, 9.2231021719790307E-002_dp, &
                   3.0711526461303582E-002_dp, 0.13026328680244428_dp, &
                   0.39345621526954666_dp]

        print *, "Test normal_distribution_pdf_dp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 741852963
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = 0.0_dp
        scale = 1.0_dp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(res == reshape(ans, [3,5])), &
            msg="normal_distribution_pdf_dp failed", warn=warn)
    end subroutine test_nor_pdf_dp

    subroutine test_nor_pdf_qp
        real(qp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(qp) :: ans(15) =                                              &
                               [0.318893112318869510791015267899888121_qp, &
                                0.318893112318869510791015267899888121_qp, &
                                0.318893112318869510791015267899888121_qp, &
                                0.187098113959298943497433916941773493_qp, &
                                0.381644127979841926072208629709878652_qp, &
                                0.284406226125877911742317226820074389_qp, &
                           3.34430779149870031392662430113341667E-0002_qp, &
                                0.152758195268195126175546835337818968_qp, &
                                0.379699032055385862322430605449234092_qp, &
                           4.55921336246420160225771160326248108E-0002_qp, &
                                0.397923009778539599932870175995362207_qp, &
                           9.22310217197903077444047527111467539E-0002_qp, &
                           3.07115264613035858248116550205509364E-0002_qp, &
                                0.130263286802444285905503889521788587_qp, &
                                0.393456215269546631592003804418546040_qp]

        print *, "Test normal_distribution_pdf_qp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 741852963
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = 0.0_qp
        scale = 1.0_qp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_pdf(x1, loc, scale)
        res(:, 2:5) = nor_pdf(x2, loc, scale)
        call check(all(res == reshape(ans, [3,5])), &
            msg="normal_distribution_pdf_qp failed", warn=warn)
    end subroutine test_nor_pdf_qp


    subroutine test_nor_cdf_sp
        real(sp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(sp) :: ans(15) =                                              &
                  [0.455823153_sp, 0.455823153_sp, 0.455823153_sp, &
                9.58156586E-03_sp, 0.168358386_sp, 0.603805304_sp, &
                   0.708373070_sp, 0.839920878_sp, 0.907748103_sp, &
                   0.649078548_sp, 0.147290438_sp, 0.213764668_sp, &
                   0.450119823_sp, 0.951398849_sp, 0.184599251_sp]

        print *, "Test normal_distribution_cdf_sp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 369147
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = -1.0_sp
        scale = 2.0_sp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(res == reshape(ans,[3,5])), &
            msg="normal_distribution_cdf_sp failed", warn=warn)
    end subroutine test_nor_cdf_sp

    subroutine test_nor_cdf_dp
        real(dp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(dp) :: ans(15) =                                              &
                      [0.45582316174760423_dp, 0.45582316174760423_dp, &
                   0.45582316174760423_dp, 9.5815716689812702E-003_dp, &
                       0.16835838141680820_dp, 0.60380530453817427_dp, &
                       0.70837308884049544_dp, 0.83992088145762245_dp, &
                       0.90774812384621895_dp, 0.64907856692732468_dp, &
                       0.14729044831032195_dp, 0.21376468631242423_dp, &
                       0.45011981357956249_dp, 0.95139888322139476_dp, &
                       0.18459927387360053_dp]

        print *, "Test normal_distribution_cdf_dp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 369147
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = -1.0_dp
        scale = 2.0_dp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(res == reshape(ans,[3,5])), &
            msg="normal_distribution_cdf_dp failed", warn=warn)
    end subroutine test_nor_cdf_dp

    subroutine test_nor_cdf_qp
        real(qp) :: x1, x2(3,4), res(3,5), loc, scale
        integer :: i, n
        integer, allocatable :: seed(:)
        real(qp) :: ans(15) =                                              &
                               [0.455823161747604213012060184525936104_qp, &
                                0.455823161747604213012060184525936104_qp, &
                                0.455823161747604213012060184525936104_qp, &
                           9.58157166898124499848957522021964825E-0003_qp, &
                                0.168358381416808208230143115794460398_qp, &
                                0.603805304538174314209276770380849180_qp, &
                                0.708373088840495419064905156162713574_qp, &
                                0.839920881457622501226824842800251743_qp, &
                                0.907748123846218982843085184254707494_qp, &
                                0.649078566927324711953547657818466491_qp, &
                                0.147290448310321952589166480597745861_qp, &
                                0.213764686312424199720243126859273705_qp, &
                                0.450119813579562469925671345273167074_qp, &
                                0.951398883221394796840511790520284706_qp, &
                                0.184599273873600506309990574458131303_qp]

        print *, "Test normal_distribution_cdf_qp"
        call random_seed(size=n)
        allocate(seed(n))
        seed(1) = 369147
        do i = 2, n
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        loc = -1.0_qp
        scale = 2.0_qp
        x1 = nor_rvs(loc, scale)
        x2 = reshape(nor_rvs(loc, scale, 12), [3,4])
        res(:,1) = nor_cdf(x1, loc, scale)
        res(:, 2:5) = nor_cdf(x2, loc, scale)
        call check(all(res == reshape(ans,[3,5])), &
            msg="normal_distribution_cdf_qp failed", warn=warn)
    end subroutine test_nor_cdf_qp


      subroutine test_binom_rvs_int8
          integer(int8) :: res(40), k, n
          integer :: i, m
          real :: p
          integer, allocatable :: seed(:)
          integer(int8) :: ans(40) =                                        &
                      [69_int8, 78_int8, 75_int8, 73_int8, 74_int8, &
                       75_int8, 72_int8, 82_int8, 82_int8, 75_int8, &
                       7_int8,  9_int8,  10_int8, 10_int8,  9_int8, &
                       5_int8,  11_int8, 10_int8,  7_int8, 12_int8, &
                       22_int8, 26_int8, 25_int8, 30_int8, 22_int8, &
                       20_int8, 18_int8, 29_int8, 24_int8, 26_int8, &
                       4_int8,  7_int8,  6_int8,  7_int8,  8_int8,  &
                       6_int8,  6_int8,  8_int8,  7_int8,  9_int8]

          print *, "Test binomial_distribution_rvs_int8"
          call random_seed(size=m)
          allocate(seed(m))
          seed(1) = 852693417
          do i = 2, m
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          integer, allocatable :: seed(:)
          integer(int16) :: ans(40) =                                        &
                      [69_int16, 78_int16, 75_int16, 73_int16, 74_int16, &
                       75_int16, 72_int16, 82_int16, 82_int16, 75_int16, &
                       7_int16,  9_int16,  10_int16, 10_int16,  9_int16, &
                       5_int16,  11_int16, 10_int16,  7_int16, 12_int16, &
                       22_int16, 26_int16, 25_int16, 30_int16, 22_int16, &
                       20_int16, 18_int16, 29_int16, 24_int16, 26_int16, &
                       4_int16,  7_int16,  6_int16,  7_int16,  8_int16,  &
                       6_int16,  6_int16,  8_int16,  7_int16,  9_int16]

          print *, "Test binomial_distribution_rvs_int16"
          call random_seed(size=m)
          allocate(seed(m))
          seed(1) = 852693417
          do i = 2, m
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          integer, allocatable :: seed(:)
          integer(int32) :: ans(40) =                                        &
                      [69_int32, 78_int32, 75_int32, 73_int32, 74_int32, &
                       75_int32, 72_int32, 82_int32, 82_int32, 75_int32, &
                       7_int32,  9_int32,  10_int32, 10_int32,  9_int32, &
                       5_int32,  11_int32, 10_int32,  7_int32, 12_int32, &
                       22_int32, 26_int32, 25_int32, 30_int32, 22_int32, &
                       20_int32, 18_int32, 29_int32, 24_int32, 26_int32, &
                       4_int32,  7_int32,  6_int32,  7_int32,  8_int32,  &
                       6_int32,  6_int32,  8_int32,  7_int32,  9_int32]

          print *, "Test binomial_distribution_rvs_int32"
          call random_seed(size=m)
          allocate(seed(m))
          seed(1) = 852693417
          do i = 2, m
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
          integer, allocatable :: seed(:)
          integer(int64) :: ans(40) =                                        &
                      [69_int64, 78_int64, 75_int64, 73_int64, 74_int64, &
                       75_int64, 72_int64, 82_int64, 82_int64, 75_int64, &
                       7_int64,  9_int64,  10_int64, 10_int64,  9_int64, &
                       5_int64,  11_int64, 10_int64,  7_int64, 12_int64, &
                       22_int64, 26_int64, 25_int64, 30_int64, 22_int64, &
                       20_int64, 18_int64, 29_int64, 24_int64, 26_int64, &
                       4_int64,  7_int64,  6_int64,  7_int64,  8_int64,  &
                       6_int64,  6_int64,  8_int64,  7_int64,  9_int64]

          print *, "Test binomial_distribution_rvs_int64"
          call random_seed(size=m)
          allocate(seed(m))
          seed(1) = 852693417
          do i = 2, m
              call random_seed(seed(i-1), seed(i))
          end do
          call random_seed(put=seed)
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
        integer, allocatable :: seed(:)
        real :: p, res(3,5)
        real :: ans(15) = [4.25703963E-03, 4.25703963E-03, 4.25703963E-03, &
                           7.78146312E-02, 4.04635631E-02, 9.87374783E-02, &
                           1.53707610E-02, 9.58787426E-02, 0.110863134,    &
                           0.114558451,    9.87374783E-02, 6.05889633E-02, &
                           2.59382036E-02, 6.05889633E-02, 8.07851329E-02]

        print *, "Test binomial_distribution_pmf_int8"
        call random_seed(size=m)
        allocate(seed(m))
        seed(1) = 630852741
        do i = 2, m
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        n = 50_int8
        p = 0.6
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int8), [3,4])
        res(:,1) = binom_pmf(x1, n, p)
        res(:, 2:5) = binom_pmf(x2, n, p)
        call check(all(res == reshape(ans, [3,5])), &
            msg="binomial_distribution_pmf_int8 failed", warn=warn)
    end subroutine test_binom_pmf_int8

    subroutine test_binom_pmf_int16
        integer(int16) :: x1, x2(3,4),  n
        integer :: i, m
        integer, allocatable :: seed(:)
        real :: p, res(3,5)
        real :: ans(15) = [4.25703963E-03, 4.25703963E-03, 4.25703963E-03, &
                           7.78146312E-02, 4.04635631E-02, 9.87374783E-02, &
                           1.53707610E-02, 9.58787426E-02, 0.110863134,    &
                           0.114558451,    9.87374783E-02, 6.05889633E-02, &
                           2.59382036E-02, 6.05889633E-02, 8.07851329E-02]

        print *, "Test binomial_distribution_pmf_int16"
        call random_seed(size=m)
        allocate(seed(m))
        seed(1) = 630852741
        do i = 2, m
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        n = 50_int16
        p = 0.6
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int16), [3,4])
        res(:,1) = binom_pmf(x1, n, p)
        res(:, 2:5) = binom_pmf(x2, n, p)
        call check(all(res == reshape(ans, [3,5])), &
            msg="binomial_distribution_pmf_int16 failed", warn=warn)
    end subroutine test_binom_pmf_int16

    subroutine test_binom_pmf_int32
        integer(int32) :: x1, x2(3,4),  n
        integer :: i, m
        integer, allocatable :: seed(:)
        real :: p, res(3,5)
        real :: ans(15) = [4.25703963E-03, 4.25703963E-03, 4.25703963E-03, &
                           7.78146312E-02, 4.04635631E-02, 9.87374783E-02, &
                           1.53707610E-02, 9.58787426E-02, 0.110863134,    &
                           0.114558451,    9.87374783E-02, 6.05889633E-02, &
                           2.59382036E-02, 6.05889633E-02, 8.07851329E-02]

        print *, "Test binomial_distribution_pmf_int32"
        call random_seed(size=m)
        allocate(seed(m))
        seed(1) = 630852741
        do i = 2, m
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        n = 50_int32
        p = 0.6
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int32), [3,4])
        res(:,1) = binom_pmf(x1, n, p)
        res(:, 2:5) = binom_pmf(x2, n, p)
        call check(all(res == reshape(ans, [3,5])), &
            msg="binomial_distribution_pmf_int32 failed", warn=warn)
    end subroutine test_binom_pmf_int32

    subroutine test_binom_pmf_int64
        integer(int64) :: x1, x2(3,4),  n
        integer :: i, m
        integer, allocatable :: seed(:)
        real :: p, res(3,5)
        real :: ans(15) = [4.25703963E-03, 4.25703963E-03, 4.25703963E-03, &
                           7.78146312E-02, 4.04635631E-02, 9.87374783E-02, &
                           1.53707610E-02, 9.58787426E-02, 0.110863134,    &
                           0.114558451,    9.87374783E-02, 6.05889633E-02, &
                           2.59382036E-02, 6.05889633E-02, 8.07851329E-02]

        print *, "Test binomial_distribution_pmf_int64"
        call random_seed(size=m)
        allocate(seed(m))
        seed(1) = 630852741
        do i = 2, m
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        n = 50_int64
        p = 0.6
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int64), [3,4])
        res(:,1) = binom_pmf(x1, n, p)
        res(:, 2:5) = binom_pmf(x2, n, p)
        call check(all(res == reshape(ans, [3,5])), &
            msg="binomial_distribution_pmf_int64 failed", warn=warn)
    end subroutine test_binom_pmf_int64


    subroutine test_binom_cdf_int8
        integer(int8) :: x1, x2(3,4),  n
        integer :: i, m
        integer, allocatable :: seed(:)
        real :: p, res(3,5)
        real :: ans(15) = [0.943473637, 0.943473637, 0.943473637, &
                           0.125598967, 0.872478724, 0.250010669, &
                           0.415892929, 0.250010669, 0.943473637, &
                           0.755337179, 0.595598698, 0.943473637, &
                           0.872478724, 0.125598967, 0.415892929]

        print *, "Test binomial_distribution_cdf_int8"
        call random_seed(size=m)
        allocate(seed(m))
        seed(1) = 17428396
        do i = 2, m
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        n = 20_int8
        p = 0.4
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int8), [3,4])
        res(:,1) = binom_cdf(x1, n, p)
        res(:, 2:5) = binom_cdf(x2, n, p)
        call check(all(res == reshape(ans, [3,5])), &
            msg="binomial_distribution_cdf_int8 failed", warn=warn)
    end subroutine test_binom_cdf_int8

    subroutine test_binom_cdf_int16
        integer(int16) :: x1, x2(3,4),  n
        integer :: i, m
        integer, allocatable :: seed(:)
        real :: p, res(3,5)
        real :: ans(15) = [0.943473637, 0.943473637, 0.943473637, &
                           0.125598967, 0.872478724, 0.250010669, &
                           0.415892929, 0.250010669, 0.943473637, &
                           0.755337179, 0.595598698, 0.943473637, &
                           0.872478724, 0.125598967, 0.415892929]

        print *, "Test binomial_distribution_cdf_int16"
        call random_seed(size=m)
        allocate(seed(m))
        seed(1) = 17428396
        do i = 2, m
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        n = 20_int16
        p = 0.4
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int16), [3,4])
        res(:,1) = binom_cdf(x1, n, p)
        res(:, 2:5) = binom_cdf(x2, n, p)
        call check(all(res == reshape(ans, [3,5])), &
            msg="binomial_distribution_cdf_int16 failed", warn=warn)
    end subroutine test_binom_cdf_int16

    subroutine test_binom_cdf_int32
        integer(int32) :: x1, x2(3,4),  n
        integer :: i, m
        integer, allocatable :: seed(:)
        real :: p, res(3,5)
        real :: ans(15) = [0.943473637, 0.943473637, 0.943473637, &
                           0.125598967, 0.872478724, 0.250010669, &
                           0.415892929, 0.250010669, 0.943473637, &
                           0.755337179, 0.595598698, 0.943473637, &
                           0.872478724, 0.125598967, 0.415892929]

        print *, "Test binomial_distribution_cdf_int32"
        call random_seed(size=m)
        allocate(seed(m))
        seed(1) = 17428396
        do i = 2, m
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        n = 20_int32
        p = 0.4
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int32), [3,4])
        res(:,1) = binom_cdf(x1, n, p)
        res(:, 2:5) = binom_cdf(x2, n, p)
        call check(all(res == reshape(ans, [3,5])), &
            msg="binomial_distribution_cdf_int32 failed", warn=warn)
    end subroutine test_binom_cdf_int32

    subroutine test_binom_cdf_int64
        integer(int64) :: x1, x2(3,4),  n
        integer :: i, m
        integer, allocatable :: seed(:)
        real :: p, res(3,5)
        real :: ans(15) = [0.943473637, 0.943473637, 0.943473637, &
                           0.125598967, 0.872478724, 0.250010669, &
                           0.415892929, 0.250010669, 0.943473637, &
                           0.755337179, 0.595598698, 0.943473637, &
                           0.872478724, 0.125598967, 0.415892929]

        print *, "Test binomial_distribution_cdf_int64"
        call random_seed(size=m)
        allocate(seed(m))
        seed(1) = 17428396
        do i = 2, m
            call random_seed(seed(i-1), seed(i))
        end do
        call random_seed(put=seed)
        n = 20_int64
        p = 0.4
        x1 = binom_rvs(n, p)
        x2 = reshape(binom_rvs(n, p, 12_int64), [3,4])
        res(:,1) = binom_cdf(x1, n, p)
        res(:, 2:5) = binom_cdf(x2, n, p)
        call check(all(res == reshape(ans, [3,5])), &
            msg="binomial_distribution_cdf_int64 failed", warn=warn)
    end subroutine test_binom_cdf_int64


end program test_distribution
