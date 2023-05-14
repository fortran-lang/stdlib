module stdlib_random
    use stdlib_kinds, only: int8, int16, int32, int64
    use stdlib_optval, only: optval
    use stdlib_error, only: error_stop
    implicit none
    private
    integer, parameter :: MAX_INT_BIT_SIZE = bit_size(1_int64)
    integer(int64) :: st(4) ! internal states for xoshiro256ss function
    integer(int64) :: si = 614872703977525537_int64 ! default seed value
    logical :: seed_initialized = .false.

    public :: random_seed
    public :: dist_rand


    interface dist_rand
    !! Version experimental
    !!
    !! Generation of random integers with different kinds
    !! ([Specification](../page/specs/stdlib_random.html#
    !! dist_rand-get-a-random-integer-with-specified-kind))
        module procedure dist_rand_iint8
        module procedure dist_rand_iint16
        module procedure dist_rand_iint32
        module procedure dist_rand_iint64
    end interface dist_rand

    interface random_seed
    !! Version experimental
    !!
    !! Set seed value for random number generator
    !! ([Specification](../page/specs/stdlib_random.html#
    !! random_seed-set-or-get-a-value-of-seed-to-the-probability-distribution-pseudorandom-number-generator))
    !!
        module procedure random_distribution_seed_iint8
        module procedure random_distribution_seed_iint16
        module procedure random_distribution_seed_iint32
        module procedure random_distribution_seed_iint64
    end interface random_seed


    contains

    function dist_rand_iint8(n) result(res)
    !! Random integer generation for various kinds
    !! result = [-2^k, 2^k - 1], k = 7, 15, 31, 63, depending on input kind
    !! Result will be operated by bitwise operators to generate desired integer
    !! and real pseudorandom numbers
    !!
        integer(int8), intent(in) :: n
        integer(int8) :: res
        integer :: k

        k = MAX_INT_BIT_SIZE - bit_size(n)
        if(k < 0) call error_stop("Error(dist_rand): Integer bit size is"       &
                  //" greater than 64bit")
        res = shiftr(xoshiro256ss( ), k)
    end function dist_rand_iint8

    function dist_rand_iint16(n) result(res)
    !! Random integer generation for various kinds
    !! result = [-2^k, 2^k - 1], k = 7, 15, 31, 63, depending on input kind
    !! Result will be operated by bitwise operators to generate desired integer
    !! and real pseudorandom numbers
    !!
        integer(int16), intent(in) :: n
        integer(int16) :: res
        integer :: k

        k = MAX_INT_BIT_SIZE - bit_size(n)
        if(k < 0) call error_stop("Error(dist_rand): Integer bit size is"       &
                  //" greater than 64bit")
        res = shiftr(xoshiro256ss( ), k)
    end function dist_rand_iint16

    function dist_rand_iint32(n) result(res)
    !! Random integer generation for various kinds
    !! result = [-2^k, 2^k - 1], k = 7, 15, 31, 63, depending on input kind
    !! Result will be operated by bitwise operators to generate desired integer
    !! and real pseudorandom numbers
    !!
        integer(int32), intent(in) :: n
        integer(int32) :: res
        integer :: k

        k = MAX_INT_BIT_SIZE - bit_size(n)
        if(k < 0) call error_stop("Error(dist_rand): Integer bit size is"       &
                  //" greater than 64bit")
        res = shiftr(xoshiro256ss( ), k)
    end function dist_rand_iint32

    function dist_rand_iint64(n) result(res)
    !! Random integer generation for various kinds
    !! result = [-2^k, 2^k - 1], k = 7, 15, 31, 63, depending on input kind
    !! Result will be operated by bitwise operators to generate desired integer
    !! and real pseudorandom numbers
    !!
        integer(int64), intent(in) :: n
        integer(int64) :: res
        integer :: k

        k = MAX_INT_BIT_SIZE - bit_size(n)
        if(k < 0) call error_stop("Error(dist_rand): Integer bit size is"       &
                  //" greater than 64bit")
        res = shiftr(xoshiro256ss( ), k)
    end function dist_rand_iint64


    function xoshiro256ss( ) result (res)
    ! Generate random 64-bit integers
    ! Written in 2018 by David Blackman and Sebastiano Vigna (vigna@acm.org)
    ! http://prng.di.unimi.it/xoshiro256starstar.c
    !
    ! This is xoshiro256** 1.0, one of our all-purpose, rock-solid
    ! generators. It has excellent (sub-ns) speed, a state (256 bits) that is
    ! large enough for any parallel application, and it passes all tests we
    ! are aware of.
    !
    ! The state must be seeded so that it is not everywhere zero. If you have
    ! a 64-bit seed, we suggest to seed a splitmix64 generator and use its
    ! output to fill st.
    !
    ! Fortran 90 version translated from C by Jim-215-Fisher
    !
        integer(int64) :: res, t

        if(.not. seed_initialized) call random_distribution_seed_iint64(si,t)
        res = ishftc(st(2) * 5, 7) * 9
        t = shiftl(st(2), 17)
        st(3) = ieor(st(3), st(1))
        st(4) = ieor(st(4), st(2))
        st(2) = ieor(st(2), st(3))
        st(1) = ieor(st(1), st(4))
        st(3) = ieor(st(3), t)
        st(4) = ishftc(st(4), 45)
    end function xoshiro256ss

    function splitmix64(s) result(res)
    ! Written in 2015 by Sebastiano Vigna (vigna@acm.org)
    ! This is a fixed-increment version of Java 8's SplittableRandom
    ! generator.
    ! See http://dx.doi.org/10.1145/2714064.2660195 and
    ! http://docs.oracle.com/javase/8/docs/api/java/util/SplittableRandom.html
    !
    ! It is a very fast generator passing BigCrush, and it can be useful if
    ! for some reason you absolutely want 64 bits of state.
    !
    ! Fortran 90 translated from C by Jim-215-Fisher
    !
        integer(int64) :: res
        integer(int64), intent(in), optional :: s
        integer(int64) :: int01 = -7046029254386353131_int64,                  &
                          int02 = -4658895280553007687_int64,                  &
                          int03 = -7723592293110705685_int64
    ! Values are converted from C unsigned integer of 0x9e3779b97f4a7c15,
    ! 0xbf58476d1ce4e5b9, 0x94d049bb133111eb

        res = optval(s, si)
        si = res + int01
        res = ieor(res, shiftr(res, 30)) * int02
        res = ieor(res, shiftr(res, 27)) * int03
        res = ieor(res, shiftr(res, 31))
    end function splitmix64

    subroutine random_distribution_seed_iint8(put, get)
    !! Set seed value for random number generator
    !!
        integer(int8), intent(in) :: put
        integer(int8), intent(out) :: get
        integer(int64) :: tmp
        integer :: i

        tmp = splitmix64(int(put, kind = int64))
        do i = 1, 10
            tmp = splitmix64( )
        end do
        do i = 1, 4
            tmp = splitmix64( )
            st(i) = tmp
        end do
        get = int(tmp, kind = int8)
        seed_initialized = .true.
    end subroutine random_distribution_seed_iint8

    subroutine random_distribution_seed_iint16(put, get)
    !! Set seed value for random number generator
    !!
        integer(int16), intent(in) :: put
        integer(int16), intent(out) :: get
        integer(int64) :: tmp
        integer :: i

        tmp = splitmix64(int(put, kind = int64))
        do i = 1, 10
            tmp = splitmix64( )
        end do
        do i = 1, 4
            tmp = splitmix64( )
            st(i) = tmp
        end do
        get = int(tmp, kind = int16)
        seed_initialized = .true.
    end subroutine random_distribution_seed_iint16

    subroutine random_distribution_seed_iint32(put, get)
    !! Set seed value for random number generator
    !!
        integer(int32), intent(in) :: put
        integer(int32), intent(out) :: get
        integer(int64) :: tmp
        integer :: i

        tmp = splitmix64(int(put, kind = int64))
        do i = 1, 10
            tmp = splitmix64( )
        end do
        do i = 1, 4
            tmp = splitmix64( )
            st(i) = tmp
        end do
        get = int(tmp, kind = int32)
        seed_initialized = .true.
    end subroutine random_distribution_seed_iint32

    subroutine random_distribution_seed_iint64(put, get)
    !! Set seed value for random number generator
    !!
        integer(int64), intent(in) :: put
        integer(int64), intent(out) :: get
        integer(int64) :: tmp
        integer :: i

        tmp = splitmix64(int(put, kind = int64))
        do i = 1, 10
            tmp = splitmix64( )
        end do
        do i = 1, 4
            tmp = splitmix64( )
            st(i) = tmp
        end do
        get = int(tmp, kind = int64)
        seed_initialized = .true.
    end subroutine random_distribution_seed_iint64

end module stdlib_random
