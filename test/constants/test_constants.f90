module test_constants
    !! Test constant values only for double precision.
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_kinds, only : dp, int32
    use stdlib_codata, only: YEAR, &
                             ALPHA_PARTICLE_ELECTRON_MASS_RATIO, &
                             ALPHA_PARTICLE_MASS, &
                             ATOMIC_MASS_CONSTANT, &
                             AVOGADRO_CONSTANT, &
                             BOLTZMANN_CONSTANT, &
                             ELECTRON_VOLT, &
                             ELEMENTARY_CHARGE, &
                             FARADAY_CONSTANT, &
                             MOLAR_MASS_CONSTANT,&
                             MOLAR_VOLUME_OF_IDEAL_GAS_273_15_K_101_325_KPA, &
                             PLANCK_CONSTANT,&
                             SPEED_OF_LIGHT_IN_VACUUM,&
                             STANDARD_ACCELERATION_OF_GRAVITY
    implicit none
    private
    public :: collect_constants

contains

!> Collect all exported unit tests
subroutine collect_constants(testsuite)
    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [new_unittest("YEAR", test_year), &
                 new_unittest("ALPHA_PARTICLE_ELECTRON_MASS_RATIO", test_ALPHA_PARTICLE_ELECTRON_MASS_RATIO),&
                 new_unittest("ALPHA_PARTICLE_MASS", test_ALPHA_PARTICLE_MASS),&
                 new_unittest("ATOMIC_MASS_CONSTANT", test_ATOMIC_MASS_CONSTANT),&
                 new_unittest("AVOGADRO_CONSTANT", test_AVOGADRO_CONSTANT),&
                 new_unittest("BOLTZMANN_CONSTANT", test_BOLTZMANN_CONSTANT),&
                 new_unittest("ELECTRON_VOLT", test_ELECTRON_VOLT),&
                 new_unittest("ELEMENTARY_CHARGE", test_ELEMENTARY_CHARGE),&
                 new_unittest("FARADAY_CONSTANT", test_FARADAY_CONSTANT),&
                 new_unittest("MOLAR_MASS_CONSTANT", test_MOLAR_MASS_CONSTANT),&
                 new_unittest("MOLAR_VOLUME_OF_IDEAL_GAS__273_15K__101_325_KPA", test_MOLAR_VOLUME_NTP),&
                 new_unittest("PLANCK_CONSTANT", test_PLANCK_CONSTANT),&
                 new_unittest("SPEED_OF_LIGHT_IN_VACUUM", test_SPEED_OF_LIGHT),&
                 new_unittest("STANDARD_ACCELERATION_OF_GRAVITY", test_STANDARD_ACCELERATION_OF_GRAVITY),&
                 new_unittest("U_ALPHA_PARTICLE_ELECTRON_MASS_RATIO", test_U_ALPHA_PARTICLE_ELECTRON_MASS_RATIO),&
                 new_unittest("U_ALPHA_PARTICLE_MASS", test_U_ALPHA_PARTICLE_MASS),&
                 new_unittest("U_ATOMIC_MASS_CONSTANT", test_U_ATOMIC_MASS_CONSTANT),&
                 new_unittest("U_AVOGADRO_CONSTANT", test_U_AVOGADRO_CONSTANT),&
                 new_unittest("U_BOLTZMANN_CONSTANT", test_U_BOLTZMANN_CONSTANT),&
                 new_unittest("U_ELECTRON_VOLT", test_U_ELECTRON_VOLT),&
                 new_unittest("U_ELEMENTARY_CHARGE", test_U_ELEMENTARY_CHARGE),&
                 new_unittest("U_FARADAY_CONSTANT", test_U_FARADAY_CONSTANT),&
                 new_unittest("U_MOLAR_MASS_CONSTANT", test_U_MOLAR_MASS_CONSTANT),&
                 new_unittest("U_MOLAR_VOLUME_OF_IDEAL_GAS__273_15K__101_325_KPA", test_U_MOLAR_VOLUME_NTP),&
                 new_unittest("U_PLANCK_CONSTANT", test_U_PLANCK_CONSTANT),&
                 new_unittest("U_SPEED_OF_LIGHT_IN_VACUUM", test_U_SPEED_OF_LIGHT),&
                 new_unittest("U_STANDARD_ACCELERATION_OF_GRAVITY", test_U_STANDARD_ACCELERATION_OF_GRAVITY)]
end subroutine

subroutine test_year(error)
    type(error_type), allocatable, intent(out) :: error 
    
    integer(int32) :: value, expected, diff
    
    expected = 2022
    value = YEAR
    diff = expected - value
    call check(error, diff, 0)
    if (allocated(error)) return
end subroutine

subroutine test_ALPHA_PARTICLE_ELECTRON_MASS_RATIO(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 7294.29954171d0 * fac
    value = ALPHA_PARTICLE_ELECTRON_MASS_RATIO%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_ALPHA_PARTICLE_MASS(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d27
    expected = 6.6446573450d-27 * fac
    value = ALPHA_PARTICLE_MASS%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_ATOMIC_MASS_CONSTANT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d27
    expected = 1.66053906892d-27 * fac
    value = ATOMIC_MASS_CONSTANT%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_AVOGADRO_CONSTANT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d-23
    expected =  6.02214076d23 * fac
    value = AVOGADRO_CONSTANT%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_BOLTZMANN_CONSTANT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d23
    expected =  1.380649d-23 * fac
    value = BOLTZMANN_CONSTANT%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_ELECTRON_VOLT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d19
    expected = 1.602176634d-19 * fac
    value = ELECTRON_VOLT%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_ELEMENTARY_CHARGE(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d19
    expected = 1.602176634d-19 * fac
    value = ELEMENTARY_CHARGE%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_FARADAY_CONSTANT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 96485.33212d0 * fac
    value = FARADAY_CONSTANT%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_MOLAR_MASS_CONSTANT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d3
    expected = 1.00000000105d-3 * fac
    value = MOLAR_MASS_CONSTANT%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_MOLAR_VOLUME_NTP(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d3
    expected = 22.41396954d-3 * fac
    value = MOLAR_VOLUME_OF_IDEAL_GAS_273_15_K_101_325_KPA%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_PLANCK_CONSTANT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d34
    expected = 6.62607015d-34 * fac
    value = PLANCK_CONSTANT%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_SPEED_OF_LIGHT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 299792458.0d0 * fac
    value = SPEED_OF_LIGHT_IN_VACUUM%to_real(fac) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_STANDARD_ACCELERATION_OF_GRAVITY(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 9.80665d0 * fac
    value = STANDARD_ACCELERATION_OF_GRAVITY%to_real(fac) * fac 
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_ALPHA_PARTICLE_ELECTRON_MASS_RATIO(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d7
    expected = 0.00000017d0 * fac
    value = ALPHA_PARTICLE_ELECTRON_MASS_RATIO%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_ALPHA_PARTICLE_MASS(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d36
    expected = 0.0000000021d-27 * fac
    value = ALPHA_PARTICLE_MASS%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_ATOMIC_MASS_CONSTANT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d37
    expected = 0.00000000052d-27 * fac
    value = ATOMIC_MASS_CONSTANT%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_AVOGADRO_CONSTANT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 0.0d0 * fac
    value = AVOGADRO_CONSTANT%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_BOLTZMANN_CONSTANT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 0.0d0 * fac
    value = BOLTZMANN_CONSTANT%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_ELECTRON_VOLT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 0.0d0 * fac
    value = ELECTRON_VOLT%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_ELEMENTARY_CHARGE(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 0.0d0 * fac
    value = ELEMENTARY_CHARGE%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_FARADAY_CONSTANT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 0.0d0 * fac
    value = FARADAY_CONSTANT%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_MOLAR_MASS_CONSTANT(error)
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d13
    expected = 0.00000000031d-3 * fac
    value = MOLAR_MASS_CONSTANT%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_MOLAR_VOLUME_NTP(error)
    implicit none
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 0.0d0 * fac
    value = MOLAR_VOLUME_OF_IDEAL_GAS_273_15_K_101_325_KPA%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_PLANCK_CONSTANT(error)
    implicit none
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 0.0d0 * fac
    value = PLANCK_CONSTANT%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_SPEED_OF_LIGHT(error)
    implicit none
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 0.0d0 * fac
    value = SPEED_OF_LIGHT_IN_VACUUM%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

subroutine test_U_STANDARD_ACCELERATION_OF_GRAVITY(error)
    implicit none
    type(error_type), allocatable, intent(out) :: error 
    real(dp) :: value, expected, diff, fac
    fac = 1.0d0
    expected = 0.0d0 * fac
    value = STANDARD_ACCELERATION_OF_GRAVITY%to_real(fac, uncertainty=.true.) * fac
    diff = expected - value
    call check(error, diff, 0.0d0)
    if (allocated(error)) return
end subroutine

end module test_constants

program tester
    use iso_fortran_env
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_constants, only : collect_constants
    implicit none
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'
    integer :: stat, is

    stat = 0

    testsuites = [new_testsuite("constants", collect_constants)]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program