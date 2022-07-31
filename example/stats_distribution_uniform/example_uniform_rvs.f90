program example_uniform_rvs
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_uniform, only: uni => rvs_uniform

  implicit none
  complex :: loc, scale
  real :: a(3, 4, 5), b(3, 4, 5)
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  print *, uni()           !real standard uniform random variate in [0., 1.]
! 0.161520019

  print *, uni(3.0)         !an uniform random variate in [0., 3.]
! 1.65974522

  print *, uni(-0.5, 1.0)   !an uniform random variate in [-0.5, 0.5]
! 0.486900032

  print *, uni(-1.0, 2.0, 10)
!an array of 10 uniform random variates in [-1., 1.]

!0.884182811  -0.771520197  0.560377002  0.709313750  -7.12267756E-02
!-0.431066573  0.497536063  -0.396331906  -0.325983286  0.137686729

  print *, uni(20)          !a random integer variate in [0, 20]
! 17

  print *, uni(5, 13)        !a random integer variate in [5, 18]
! 15

  print *, uni(3, 19, 10)     !an array of 10 integer variates in [3,22]

!7  16  16  12  9  21  19  4  3  19

  loc = (-0.5, -0.5)
  scale = (1.0, 1.0)

  print *, uni(scale)       !a complex uniform random variate in unit square

!(0.139202669, 0.361759573)

  print *, uni(loc, scale)
!a complex uniform random variate in [(-0.5, -0.5), (0.5, 0.5)]

!(0.296536088,-0.143987954)

  print *, uni(loc, scale, 10)
!an array of 10 complex uniform random variate in [(-0.5, -0.5), (0.5, 0.5)]

!(-0.302334785,-0.401923567)    (0.281620383,9.534919262E-02)
! (-0.374348879,0.457528770)     (0.442990601,-0.240510434)
! (-0.421572685,0.279313922)     (-0.182090610,5.901372433E-02)
! (-7.864198089E-02,0.378484428)    (-0.423258364,-0.201292425)
! (0.193327367,-0.353985727)    (-0.397661150,0.355926156)

  a(:, :, :) = -0.5
  b(:, :, :) = 1.0

  print *, uni(a, b)
!a rank 3 array of random variates in [-0.5,0.5]

! -0.249188632   -0.199248433   -0.389813602   2.88307667E-03   0.238479793,
!  0.264856219   -0.205177426   -0.480921626   0.131218433   0.252170086,
! -0.303151041   -8.89462233E-02   -0.377370685   0.341802299   0.323204756,
! 0.358679056   -0.138909757   0.384329498   -0.109372199   0.132353067,
! 0.494320452   0.419343710   -0.103044361   0.461389005   0.403132677
! 0.121850729   0.403839290   -0.349389791   0.490482628   0.156600773
! 8.46788883E-02   -0.483680278   0.388107836   0.119698405   0.154214382
! 0.153113484   0.236523747   0.155937552   -0.135760903   0.219589531
! 0.394639254   6.30156994E-02   -0.342692465   -0.444846451   -0.215700030
! 0.204189956   -0.208748132   0.355063021   8.98272395E-02   -0.237928331
! 2.98077464E-02   -0.485149682   -8.06870461E-02   -0.372713923
! -0.178335011   0.283877611   -2.13934183E-02   -9.21690464E-03
! 4.56320047E-02   0.220112979

end program example_uniform_rvs
