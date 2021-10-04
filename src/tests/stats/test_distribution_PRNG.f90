program test_distribution_PRNG
    use stdlib_error, only : check
    use stdlib_kinds, only: int8, int16, int32, int64
    use stdlib_random, only : random_seed, dist_rand

    implicit none
    logical ::  warn = .true.

    call test_random_seed
    call test_random_rand_iint8
    call test_random_rand_iint16
    call test_random_rand_iint32
    call test_random_rand_iint64


    contains

    subroutine test_random_seed
        integer :: put, get, res(5)
        integer :: ans(5) = [-1859553078, -1933696596, -642834430, &
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
        call check(all(res == ans), msg="random seed test failed.",warn=warn)
    end subroutine test_random_seed

    subroutine test_random_rand_iint8
        integer :: put, get, i

        integer(int8) :: res(5), ans(5)=[118, -15, -72, 101, 70]


        print *, ""
        print *, "Test random_rand with kind int8"
        put = 12345678
        call random_seed(put, get)
        do i = 1, 5
            res(i) = dist_rand(1_int8)
        end do
        call check(all(res == ans), msg="random_rand with kind int8 test" &
                                     //" failed.", warn=warn)
    end subroutine test_random_rand_iint8

    subroutine test_random_rand_iint16
        integer :: put, get, i

        integer(int16) :: res(5), ans(5)=[30286, -3799, -18204, 25947, 18148]


        print *, ""
        print *, "Test random_rand with kind int16"
        put = 12345678
        call random_seed(put, get)
        do i = 1, 5
            res(i) = dist_rand(1_int16)
        end do
        call check(all(res == ans), msg="random_rand with kind int16 test" &
                                     //" failed.", warn=warn)
    end subroutine test_random_rand_iint16

    subroutine test_random_rand_iint32
        integer :: put, get, i

        integer(int32) :: res(5), ans(5)=[1984865646, -248954393, -1192993267, &
                                  1700514835, 1189401802]


        print *, ""
        print *, "Test random_rand with kind int32"
        put = 12345678
        call random_seed(put, get)
        do i = 1, 5
            res(i) = dist_rand(1_int32)
        end do
        call check(all(res == ans), msg="random_rand with kind int32 test" &
                                     //" failed.", warn=warn)
    end subroutine test_random_rand_iint32

    subroutine test_random_rand_iint64
        integer :: put, get, i

        integer(int64) :: res(5), ans(5)=[8524933037632333570_int64, &
                                  -1069250973542918798_int64,        &
                                  -5123867065024149335_int64,        &
                                  7303655603304982073_int64,         &
                                  5108441843522503546_int64]


        print *, ""
        print *, "Test random_rand with kind int64"
        put = 12345678
        call random_seed(put, get)
        do i = 1, 5
            res(i) = dist_rand(1_int64)
        end do
        call check(all(res == ans), msg="random_rand with kind int64 test" &
                                     //" failed.", warn=warn)
    end subroutine test_random_rand_iint64

end program test_distribution_PRNG
