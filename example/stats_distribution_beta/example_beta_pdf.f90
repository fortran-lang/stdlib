program example_beta_pdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_beta, only: rbeta => rvs_beta, &
                                             beta_pdf => pdf_beta

  implicit none
  real :: x, a, b
  real :: xarr(3, 4, 5)
  integer :: seed_put, seed_get, i

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  a = 2.0; b = 5.0

  ! probability density at x=0.3 for beta(2,5) distribution
  print *, beta_pdf(0.3, a, b)
  ! 1.32859993

  ! probability density at x=1.3 with loc=1.0 for beta(2,5) distribution
  print *, beta_pdf(1.3, a, b, 1.0)
  ! 1.32859993

  ! generate random variates and compute their pdf
  do i = 1, 60
    xarr(i, 1, 1) = rbeta(a, b)
  end do

  print *, beta_pdf(xarr, a, b)
  ! 1.23914337  0.925268888  0.969913423  0.998693407  1.44889069
  ! 0.331506640  0.988773048  1.41048801  0.502468705  0.564825535
  ! 0.661424160  0.931534827  0.861649513  1.01826906  1.44227338
  ! 0.782453180  0.145308748  0.841068327  1.49033546  0.727149904
  ! 1.14145494  0.905850768  1.08809102  1.40226245  ... (60 elements total)

end program example_beta_pdf
