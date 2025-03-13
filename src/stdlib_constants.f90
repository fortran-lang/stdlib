
module stdlib_constants
    !! Constants
    !! ([Specification](../page/specs/stdlib_constants.html))
    use stdlib_kinds
    use stdlib_codata, only: SPEED_OF_LIGHT_IN_VACUUM, &
                             VACUUM_ELECTRIC_PERMITTIVITY, &
                             VACUUM_MAG_PERMEABILITY, &
                             PLANCK_CONSTANT, &
                             NEWTONIAN_CONSTANT_OF_GRAVITATION, &
                             STANDARD_ACCELERATION_OF_GRAVITY, &
                             ELEMENTARY_CHARGE, &
                             MOLAR_GAS_CONSTANT, &
                             FINE_STRUCTURE_CONSTANT, &
                             AVOGADRO_CONSTANT, &
                             BOLTZMANN_CONSTANT, &
                             STEFAN_BOLTZMANN_CONSTANT, &
                             WIEN_WAVELENGTH_DISPLACEMENT_LAW_CONSTANT, &
                             RYDBERG_CONSTANT, &
                             ELECTRON_MASS, &
                             PROTON_MASS, &
                             NEUTRON_MASS, &
                             ATOMIC_MASS_CONSTANT
    private

    ! mathematical constants
    real(sp), parameter, public :: PI_sp = acos(-1.0_sp) !! PI
    real(dp), parameter, public :: PI_dp = acos(-1.0_dp) !! PI
    
    ! Physical constants
    real(dp), parameter, public :: c = SPEED_OF_LIGHT_IN_VACUUM%value !! Speed of light in vacuum
    real(dp), parameter, public :: speed_of_light = SPEED_OF_LIGHT_IN_VACUUM%value !! Speed of light in vacuum
    real(dp), parameter, public :: mu_0 = VACUUM_MAG_PERMEABILITY%value !! vacuum mag. permeability
    real(dp), parameter, public :: epsilon_0 = VACUUM_ELECTRIC_PERMITTIVITY%value !! vacuum mag. permeability
    real(dp), parameter, public :: h = PLANCK_CONSTANT%value !! Planck constant
    real(dp), parameter, public :: Planck = PLANCK_CONSTANT%value !! Planck constant
    real(dp), parameter, public :: hbar = PLANCK_CONSTANT%value / (2.0_dp * PI_dp) !! Reduced Planck constant
    real(dp), parameter, public :: G = NEWTONIAN_CONSTANT_OF_GRAVITATION%value !! Newtonian constant of gravitation
    real(dp), parameter, public :: gravitation_constant = NEWTONIAN_CONSTANT_OF_GRAVITATION%value !! Newtonian constant of gravitation
    real(dp), parameter, public :: g2 = STANDARD_ACCELERATION_OF_GRAVITY%value !! Standard acceleration of gravity
    real(dp), parameter, public :: e = ELEMENTARY_CHARGE%value !! Elementary charge
    real(dp), parameter, public :: R = MOLAR_GAS_CONSTANT%value !! Molar gas constant
    real(dp), parameter, public :: gas_constant = MOLAR_GAS_CONSTANT%value !! Molar gas constant 
    real(dp), parameter, public :: alpha = FINE_STRUCTURE_CONSTANT%value !! Fine structure constant
    real(dp), parameter, public :: fine_structure = FINE_STRUCTURE_CONSTANT%value !! Fine structure constant
    real(dp), parameter, public :: N_A  = AVOGADRO_CONSTANT%value !! Avogadro constant
    real(dp), parameter, public :: Avogadro = AVOGADRO_CONSTANT%value !! Avogadro constant
    real(dp), parameter, public :: k = BOLTZMANN_CONSTANT%value !! Boltzmann constant
    real(dp), parameter, public :: Boltzmann = BOLTZMANN_CONSTANT%value !! Boltzmann constant
    real(dp), parameter, public :: sigma = STEFAN_BOLTZMANN_CONSTANT%value !! Stefan-Boltzmann constant
    real(dp), parameter, public :: Stefan_Boltzmann = STEFAN_BOLTZMANN_CONSTANT%value !! Stefan-Boltzmann constant
    real(dp), parameter, public :: Wien = WIEN_WAVELENGTH_DISPLACEMENT_LAW_CONSTANT%value !! Wien wavelength displacement law constant
    real(dp), parameter, public :: Rydberg = RYDBERG_CONSTANT%value !! Rydberg constant
    real(dp), parameter, public :: m_e = ELECTRON_MASS%value !! Electron mass
    real(dp), parameter, public :: m_p = PROTON_MASS%value !! Proton mass
    real(dp), parameter, public :: m_n = NEUTRON_MASS%value !! Neutron mass
    real(dp), parameter, public :: m_u = ATOMIC_MASS_CONSTANT%value !! Atomic mass constant
    real(dp), parameter, public :: u = ATOMIC_MASS_CONSTANT%value !! Atomic mass constant

    ! Additional constants if needed
    integer(int8), parameter, public :: zero_int8 = 0_int8
    integer(int8), parameter, public :: one_int8  = 1_int8
    integer(int16), parameter, public :: zero_int16 = 0_int16
    integer(int16), parameter, public :: one_int16  = 1_int16
    integer(int32), parameter, public :: zero_int32 = 0_int32
    integer(int32), parameter, public :: one_int32  = 1_int32
    integer(int64), parameter, public :: zero_int64 = 0_int64
    integer(int64), parameter, public :: one_int64  = 1_int64
    real(sp), parameter, public :: zero_sp = 0._sp
    real(sp), parameter, public :: one_sp  = 1._sp
    real(dp), parameter, public :: zero_dp = 0._dp
    real(dp), parameter, public :: one_dp  = 1._dp
    complex(sp), parameter, public :: zero_csp = (0._sp,0._sp)
    complex(sp), parameter, public :: one_csp  = (1._sp,0._sp)
    complex(dp), parameter, public :: zero_cdp = (0._dp,0._dp)
    complex(dp), parameter, public :: one_cdp  = (1._dp,0._dp)

end module stdlib_constants
