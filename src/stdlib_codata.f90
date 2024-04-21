module stdlib_codata
    !! Codata Constants
    use stdlib_kinds, only: dp, int32
    private

type, public :: codata_constant
!! Derived type for representing a Codata constant.
    character(len=64) :: name
    real(dp) :: value
    real(dp) :: uncertainty
    character(len=32) :: unit
end type

integer(int32), parameter, public :: YEAR = 2018 !! Year of release.

type(codata_constant), parameter, public :: ALPHA_PARTICLE_ELECTRON_MASS_RATIO = &
codata_constant("alpha particle-electron mass ratio", &
real(7294.29954142d0,dp), real(0.00000024d0,dp), &
"") !! alpha particle-electron mass ratio

type(codata_constant), parameter, public :: ALPHA_PARTICLE_MASS = &
codata_constant("alpha particle mass", &
real(6.6446573357d-27,dp), real(0.0000000020d-27,dp), &
"kg") !! alpha particle mass

type(codata_constant), parameter, public :: ALPHA_PARTICLE_MASS_ENERGY_EQUIVALENT = &
codata_constant("alpha particle mass energy equivalent", &
real(5.9719201914d-10,dp), real(0.0000000018d-10,dp), &
"J") !! alpha particle mass energy equivalent

type(codata_constant), parameter, public :: ALPHA_PARTICLE_MASS_ENERGY_EQUIVALENT_IN_MEV = &
codata_constant("alpha particle mass energy equivalent in MeV", &
real(3727.3794066d0,dp), real(0.0000011d0,dp), &
"MeV") !! alpha particle mass energy equivalent in MeV

type(codata_constant), parameter, public :: ALPHA_PARTICLE_MASS_IN_U = &
codata_constant("alpha particle mass in u", &
real(4.001506179127d0,dp), real(0.000000000063d0,dp), &
"u") !! alpha particle mass in u

type(codata_constant), parameter, public :: ALPHA_PARTICLE_MOLAR_MASS = &
codata_constant("alpha particle molar mass", &
real(4.0015061777d-3,dp), real(0.0000000012d-3,dp), &
"kg mol^-1") !! alpha particle molar mass

type(codata_constant), parameter, public :: ALPHA_PARTICLE_PROTON_MASS_RATIO = &
codata_constant("alpha particle-proton mass ratio", &
real(3.97259969009d0,dp), real(0.00000000022d0,dp), &
"") !! alpha particle-proton mass ratio

type(codata_constant), parameter, public :: ALPHA_PARTICLE_RELATIVE_ATOMIC_MASS = &
codata_constant("alpha particle relative atomic mass", &
real(4.001506179127d0,dp), real(0.000000000063d0,dp), &
"") !! alpha particle relative atomic mass

type(codata_constant), parameter, public :: ANGSTROM_STAR = &
codata_constant("Angstrom star", &
real(1.00001495d-10,dp), real(0.00000090d-10,dp), &
"m") !! Angstrom star

type(codata_constant), parameter, public :: ATOMIC_MASS_CONSTANT = &
codata_constant("atomic mass constant", &
real(1.66053906660d-27,dp), real(0.00000000050d-27,dp), &
"kg") !! atomic mass constant

type(codata_constant), parameter, public :: ATOMIC_MASS_CONSTANT_ENERGY_EQUIVALENT = &
codata_constant("atomic mass constant energy equivalent", &
real(1.49241808560d-10,dp), real(0.00000000045d-10,dp), &
"J") !! atomic mass constant energy equivalent

type(codata_constant), parameter, public :: ATOMIC_MASS_CONSTANT_ENERGY_EQUIVALENT_IN_MEV = &
codata_constant("atomic mass constant energy equivalent in MeV", &
real(931.49410242d0,dp), real(0.00000028d0,dp), &
"MeV") !! atomic mass constant energy equivalent in MeV

type(codata_constant), parameter, public :: ATOMIC_MASS_UNIT_ELECTRON_VOLT_RELATIONSHIP = &
codata_constant("atomic mass unit-electron volt relationship", &
real(9.3149410242d8,dp), real(0.0000000028d8,dp), &
"eV") !! atomic mass unit-electron volt relationship

type(codata_constant), parameter, public :: ATOMIC_MASS_UNIT_HARTREE_RELATIONSHIP = &
codata_constant("atomic mass unit-hartree relationship", &
real(3.4231776874d7,dp), real(0.0000000010d7,dp), &
"E_h") !! atomic mass unit-hartree relationship

type(codata_constant), parameter, public :: ATOMIC_MASS_UNIT_HERTZ_RELATIONSHIP = &
codata_constant("atomic mass unit-hertz relationship", &
real(2.25234271871d23,dp), real(0.00000000068d23,dp), &
"Hz") !! atomic mass unit-hertz relationship

type(codata_constant), parameter, public :: ATOMIC_MASS_UNIT_INVERSE_METER_RELATIONSHIP = &
codata_constant("atomic mass unit-inverse meter relationship", &
real(7.5130066104d14,dp), real(0.0000000023d14,dp), &
"m^-1") !! atomic mass unit-inverse meter relationship

type(codata_constant), parameter, public :: ATOMIC_MASS_UNIT_JOULE_RELATIONSHIP = &
codata_constant("atomic mass unit-joule relationship", &
real(1.49241808560d-10,dp), real(0.00000000045d-10,dp), &
"J") !! atomic mass unit-joule relationship

type(codata_constant), parameter, public :: ATOMIC_MASS_UNIT_KELVIN_RELATIONSHIP = &
codata_constant("atomic mass unit-kelvin relationship", &
real(1.08095401916d13,dp), real(0.00000000033d13,dp), &
"K") !! atomic mass unit-kelvin relationship

type(codata_constant), parameter, public :: ATOMIC_MASS_UNIT_KILOGRAM_RELATIONSHIP = &
codata_constant("atomic mass unit-kilogram relationship", &
real(1.66053906660d-27,dp), real(0.00000000050d-27,dp), &
"kg") !! atomic mass unit-kilogram relationship

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_1ST_HYPERPOLARIZABILITY = &
codata_constant("atomic unit of 1st hyperpolarizability", &
real(3.2063613061d-53,dp), real(0.0000000015d-53,dp), &
"C^3 m^3 J^-2") !! atomic unit of 1st hyperpolarizability

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_2ND_HYPERPOLARIZABILITY = &
codata_constant("atomic unit of 2nd hyperpolarizability", &
real(6.2353799905d-65,dp), real(0.0000000038d-65,dp), &
"C^4 m^4 J^-3") !! atomic unit of 2nd hyperpolarizability

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_ACTION = &
codata_constant("atomic unit of action", &
real(1.054571817d-34,dp), real(0.0d0,dp), &
"J s") !! atomic unit of action

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_CHARGE = &
codata_constant("atomic unit of charge", &
real(1.602176634d-19,dp), real(0.0d0,dp), &
"C") !! atomic unit of charge

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_CHARGE_DENSITY = &
codata_constant("atomic unit of charge density", &
real(1.08120238457d12,dp), real(0.00000000049d12,dp), &
"C m^-3") !! atomic unit of charge density

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_CURRENT = &
codata_constant("atomic unit of current", &
real(6.623618237510d-3,dp), real(0.000000000013d-3,dp), &
"A") !! atomic unit of current

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_ELECTRIC_DIPOLE_MOM = &
codata_constant("atomic unit of electric dipole mom.", &
real(8.4783536255d-30,dp), real(0.0000000013d-30,dp), &
"C m") !! atomic unit of electric dipole mom.

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_ELECTRIC_FIELD = &
codata_constant("atomic unit of electric field", &
real(5.14220674763d11,dp), real(0.00000000078d11,dp), &
"V m^-1") !! atomic unit of electric field

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_ELECTRIC_FIELD_GRADIENT = &
codata_constant("atomic unit of electric field gradient", &
real(9.7173624292d21,dp), real(0.0000000029d21,dp), &
"V m^-2") !! atomic unit of electric field gradient

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_ELECTRIC_POLARIZABILITY = &
codata_constant("atomic unit of electric polarizability", &
real(1.64877727436d-41,dp), real(0.00000000050d-41,dp), &
"C^2 m^2 J^-1") !! atomic unit of electric polarizability

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_ELECTRIC_POTENTIAL = &
codata_constant("atomic unit of electric potential", &
real(27.211386245988d0,dp), real(0.000000000053d0,dp), &
"V") !! atomic unit of electric potential

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_ELECTRIC_QUADRUPOLE_MOM = &
codata_constant("atomic unit of electric quadrupole mom.", &
real(4.4865515246d-40,dp), real(0.0000000014d-40,dp), &
"C m^2") !! atomic unit of electric quadrupole mom.

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_ENERGY = &
codata_constant("atomic unit of energy", &
real(4.3597447222071d-18,dp), real(0.0000000000085d-18,dp), &
"J") !! atomic unit of energy

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_FORCE = &
codata_constant("atomic unit of force", &
real(8.2387234983d-8,dp), real(0.0000000012d-8,dp), &
"N") !! atomic unit of force

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_LENGTH = &
codata_constant("atomic unit of length", &
real(5.29177210903d-11,dp), real(0.00000000080d-11,dp), &
"m") !! atomic unit of length

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_MAG_DIPOLE_MOM = &
codata_constant("atomic unit of mag. dipole mom.", &
real(1.85480201566d-23,dp), real(0.00000000056d-23,dp), &
"J T^-1") !! atomic unit of mag. dipole mom.

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_MAG_FLUX_DENSITY = &
codata_constant("atomic unit of mag. flux density", &
real(2.35051756758d5,dp), real(0.00000000071d5,dp), &
"T") !! atomic unit of mag. flux density

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_MAGNETIZABILITY = &
codata_constant("atomic unit of magnetizability", &
real(7.8910366008d-29,dp), real(0.0000000048d-29,dp), &
"J T^-2") !! atomic unit of magnetizability

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_MASS = &
codata_constant("atomic unit of mass", &
real(9.1093837015d-31,dp), real(0.0000000028d-31,dp), &
"kg") !! atomic unit of mass

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_MOMENTUM = &
codata_constant("atomic unit of momentum", &
real(1.99285191410d-24,dp), real(0.00000000030d-24,dp), &
"kg m s^-1") !! atomic unit of momentum

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_PERMITTIVITY = &
codata_constant("atomic unit of permittivity", &
real(1.11265005545d-10,dp), real(0.00000000017d-10,dp), &
"F m^-1") !! atomic unit of permittivity

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_TIME = &
codata_constant("atomic unit of time", &
real(2.4188843265857d-17,dp), real(0.0000000000047d-17,dp), &
"s") !! atomic unit of time

type(codata_constant), parameter, public :: ATOMIC_UNIT_OF_VELOCITY = &
codata_constant("atomic unit of velocity", &
real(2.18769126364d6,dp), real(0.00000000033d6,dp), &
"m s^-1") !! atomic unit of velocity

type(codata_constant), parameter, public :: AVOGADRO_CONSTANT = &
codata_constant("Avogadro constant", &
real(6.02214076d23,dp), real(0.0d0,dp), &
"mol^-1") !! Avogadro constant

type(codata_constant), parameter, public :: BOHR_MAGNETON = &
codata_constant("Bohr magneton", &
real(9.2740100783d-24,dp), real(0.0000000028d-24,dp), &
"J T^-1") !! Bohr magneton

type(codata_constant), parameter, public :: BOHR_MAGNETON_IN_EV_T = &
codata_constant("Bohr magneton in eV/T", &
real(5.7883818060d-5,dp), real(0.0000000017d-5,dp), &
"eV T^-1") !! Bohr magneton in eV/T

type(codata_constant), parameter, public :: BOHR_MAGNETON_IN_HZ_T = &
codata_constant("Bohr magneton in Hz/T", &
real(1.39962449361d10,dp), real(0.00000000042d10,dp), &
"Hz T^-1") !! Bohr magneton in Hz/T

type(codata_constant), parameter, public :: BOHR_MAGNETON_IN_INVERSE_METER_PER_TESLA = &
codata_constant("Bohr magneton in inverse meter per tesla", &
real(46.686447783d0,dp), real(0.000000014d0,dp), &
"m^-1 T^-1") !! Bohr magneton in inverse meter per tesla

type(codata_constant), parameter, public :: BOHR_MAGNETON_IN_K_T = &
codata_constant("Bohr magneton in K/T", &
real(0.67171381563d0,dp), real(0.00000000020d0,dp), &
"K T^-1") !! Bohr magneton in K/T

type(codata_constant), parameter, public :: BOHR_RADIUS = &
codata_constant("Bohr radius", &
real(5.29177210903d-11,dp), real(0.00000000080d-11,dp), &
"m") !! Bohr radius

type(codata_constant), parameter, public :: BOLTZMANN_CONSTANT = &
codata_constant("Boltzmann constant", &
real(1.380649d-23,dp), real(0.0d0,dp), &
"J K^-1") !! Boltzmann constant

type(codata_constant), parameter, public :: BOLTZMANN_CONSTANT_IN_EV_K = &
codata_constant("Boltzmann constant in eV/K", &
real(8.617333262d-5,dp), real(0.0d0,dp), &
"eV K^-1") !! Boltzmann constant in eV/K

type(codata_constant), parameter, public :: BOLTZMANN_CONSTANT_IN_HZ_K = &
codata_constant("Boltzmann constant in Hz/K", &
real(2.083661912d10,dp), real(0.0d0,dp), &
"Hz K^-1") !! Boltzmann constant in Hz/K

type(codata_constant), parameter, public :: BOLTZMANN_CONSTANT_IN_INVERSE_METER_PER_KELVIN = &
codata_constant("Boltzmann constant in inverse meter per kelvin", &
real(69.50348004d0,dp), real(0.0d0,dp), &
"m^-1 K^-1") !! Boltzmann constant in inverse meter per kelvin

type(codata_constant), parameter, public :: CHARACTERISTIC_IMPEDANCE_OF_VACUUM = &
codata_constant("characteristic impedance of vacuum", &
real(376.730313668d0,dp), real(0.000000057d0,dp), &
"ohm") !! characteristic impedance of vacuum

type(codata_constant), parameter, public :: CLASSICAL_ELECTRON_RADIUS = &
codata_constant("classical electron radius", &
real(2.8179403262d-15,dp), real(0.0000000013d-15,dp), &
"m") !! classical electron radius

type(codata_constant), parameter, public :: COMPTON_WAVELENGTH = &
codata_constant("Compton wavelength", &
real(2.42631023867d-12,dp), real(0.00000000073d-12,dp), &
"m") !! Compton wavelength

type(codata_constant), parameter, public :: CONDUCTANCE_QUANTUM = &
codata_constant("conductance quantum", &
real(7.748091729d-5,dp), real(0.0d0,dp), &
"S") !! conductance quantum

type(codata_constant), parameter, public :: CONVENTIONAL_VALUE_OF_AMPERE_90 = &
codata_constant("conventional value of ampere-90", &
real(1.00000008887d0,dp), real(0.0d0,dp), &
"A") !! conventional value of ampere-90

type(codata_constant), parameter, public :: CONVENTIONAL_VALUE_OF_COULOMB_90 = &
codata_constant("conventional value of coulomb-90", &
real(1.00000008887d0,dp), real(0.0d0,dp), &
"C") !! conventional value of coulomb-90

type(codata_constant), parameter, public :: CONVENTIONAL_VALUE_OF_FARAD_90 = &
codata_constant("conventional value of farad-90", &
real(0.99999998220d0,dp), real(0.0d0,dp), &
"F") !! conventional value of farad-90

type(codata_constant), parameter, public :: CONVENTIONAL_VALUE_OF_HENRY_90 = &
codata_constant("conventional value of henry-90", &
real(1.00000001779d0,dp), real(0.0d0,dp), &
"H") !! conventional value of henry-90

type(codata_constant), parameter, public :: CONVENTIONAL_VALUE_OF_JOSEPHSON_CONSTANT = &
codata_constant("conventional value of Josephson constant", &
real(483597.9d9,dp), real(0.0d0,dp), &
"Hz V^-1") !! conventional value of Josephson constant

type(codata_constant), parameter, public :: CONVENTIONAL_VALUE_OF_OHM_90 = &
codata_constant("conventional value of ohm-90", &
real(1.00000001779d0,dp), real(0.0d0,dp), &
"ohm") !! conventional value of ohm-90

type(codata_constant), parameter, public :: CONVENTIONAL_VALUE_OF_VOLT_90 = &
codata_constant("conventional value of volt-90", &
real(1.00000010666d0,dp), real(0.0d0,dp), &
"V") !! conventional value of volt-90

type(codata_constant), parameter, public :: CONVENTIONAL_VALUE_OF_VON_KLITZING_CONSTANT = &
codata_constant("conventional value of von Klitzing constant", &
real(25812.807d0,dp), real(0.0d0,dp), &
"ohm") !! conventional value of von Klitzing constant

type(codata_constant), parameter, public :: CONVENTIONAL_VALUE_OF_WATT_90 = &
codata_constant("conventional value of watt-90", &
real(1.00000019553d0,dp), real(0.0d0,dp), &
"W") !! conventional value of watt-90

type(codata_constant), parameter, public :: COPPER_X_UNIT = &
codata_constant("Copper x unit", &
real(1.00207697d-13,dp), real(0.00000028d-13,dp), &
"m") !! Copper x unit

type(codata_constant), parameter, public :: DEUTERON_ELECTRON_MAG_MOM_RATIO = &
codata_constant("deuteron-electron mag. mom. ratio", &
real(-4.664345551d-4,dp), real(0.000000012d-4,dp), &
"") !! deuteron-electron mag. mom. ratio

type(codata_constant), parameter, public :: DEUTERON_ELECTRON_MASS_RATIO = &
codata_constant("deuteron-electron mass ratio", &
real(3670.48296788d0,dp), real(0.00000013d0,dp), &
"") !! deuteron-electron mass ratio

type(codata_constant), parameter, public :: DEUTERON_G_FACTOR = &
codata_constant("deuteron g factor", &
real(0.8574382338d0,dp), real(0.0000000022d0,dp), &
"") !! deuteron g factor

type(codata_constant), parameter, public :: DEUTERON_MAG_MOM = &
codata_constant("deuteron mag. mom.", &
real(4.330735094d-27,dp), real(0.000000011d-27,dp), &
"J T^-1") !! deuteron mag. mom.

type(codata_constant), parameter, public :: DEUTERON_MAG_MOM_TO_BOHR_MAGNETON_RATIO = &
codata_constant("deuteron mag. mom. to Bohr magneton ratio", &
real(4.669754570d-4,dp), real(0.000000012d-4,dp), &
"") !! deuteron mag. mom. to Bohr magneton ratio

type(codata_constant), parameter, public :: DEUTERON_MAG_MOM_TO_NUCLEAR_MAGNETON_RATIO = &
codata_constant("deuteron mag. mom. to nuclear magneton ratio", &
real(0.8574382338d0,dp), real(0.0000000022d0,dp), &
"") !! deuteron mag. mom. to nuclear magneton ratio

type(codata_constant), parameter, public :: DEUTERON_MASS = &
codata_constant("deuteron mass", &
real(3.3435837724d-27,dp), real(0.0000000010d-27,dp), &
"kg") !! deuteron mass

type(codata_constant), parameter, public :: DEUTERON_MASS_ENERGY_EQUIVALENT = &
codata_constant("deuteron mass energy equivalent", &
real(3.00506323102d-10,dp), real(0.00000000091d-10,dp), &
"J") !! deuteron mass energy equivalent

type(codata_constant), parameter, public :: DEUTERON_MASS_ENERGY_EQUIVALENT_IN_MEV = &
codata_constant("deuteron mass energy equivalent in MeV", &
real(1875.61294257d0,dp), real(0.00000057d0,dp), &
"MeV") !! deuteron mass energy equivalent in MeV

type(codata_constant), parameter, public :: DEUTERON_MASS_IN_U = &
codata_constant("deuteron mass in u", &
real(2.013553212745d0,dp), real(0.000000000040d0,dp), &
"u") !! deuteron mass in u

type(codata_constant), parameter, public :: DEUTERON_MOLAR_MASS = &
codata_constant("deuteron molar mass", &
real(2.01355321205d-3,dp), real(0.00000000061d-3,dp), &
"kg mol^-1") !! deuteron molar mass

type(codata_constant), parameter, public :: DEUTERON_NEUTRON_MAG_MOM_RATIO = &
codata_constant("deuteron-neutron mag. mom. ratio", &
real(-0.44820653d0,dp), real(0.00000011d0,dp), &
"") !! deuteron-neutron mag. mom. ratio

type(codata_constant), parameter, public :: DEUTERON_PROTON_MAG_MOM_RATIO = &
codata_constant("deuteron-proton mag. mom. ratio", &
real(0.30701220939d0,dp), real(0.00000000079d0,dp), &
"") !! deuteron-proton mag. mom. ratio

type(codata_constant), parameter, public :: DEUTERON_PROTON_MASS_RATIO = &
codata_constant("deuteron-proton mass ratio", &
real(1.99900750139d0,dp), real(0.00000000011d0,dp), &
"") !! deuteron-proton mass ratio

type(codata_constant), parameter, public :: DEUTERON_RELATIVE_ATOMIC_MASS = &
codata_constant("deuteron relative atomic mass", &
real(2.013553212745d0,dp), real(0.000000000040d0,dp), &
"") !! deuteron relative atomic mass

type(codata_constant), parameter, public :: DEUTERON_RMS_CHARGE_RADIUS = &
codata_constant("deuteron rms charge radius", &
real(2.12799d-15,dp), real(0.00074d-15,dp), &
"m") !! deuteron rms charge radius

type(codata_constant), parameter, public :: ELECTRON_CHARGE_TO_MASS_QUOTIENT = &
codata_constant("electron charge to mass quotient", &
real(-1.75882001076d11,dp), real(0.00000000053d11,dp), &
"C kg^-1") !! electron charge to mass quotient

type(codata_constant), parameter, public :: ELECTRON_DEUTERON_MAG_MOM_RATIO = &
codata_constant("electron-deuteron mag. mom. ratio", &
real(-2143.9234915d0,dp), real(0.0000056d0,dp), &
"") !! electron-deuteron mag. mom. ratio

type(codata_constant), parameter, public :: ELECTRON_DEUTERON_MASS_RATIO = &
codata_constant("electron-deuteron mass ratio", &
real(2.724437107462d-4,dp), real(0.000000000096d-4,dp), &
"") !! electron-deuteron mass ratio

type(codata_constant), parameter, public :: ELECTRON_G_FACTOR = &
codata_constant("electron g factor", &
real(-2.00231930436256d0,dp), real(0.00000000000035d0,dp), &
"") !! electron g factor

type(codata_constant), parameter, public :: ELECTRON_GYROMAG_RATIO = &
codata_constant("electron gyromag. ratio", &
real(1.76085963023d11,dp), real(0.00000000053d11,dp), &
"s^-1 T^-1") !! electron gyromag. ratio

type(codata_constant), parameter, public :: ELECTRON_GYROMAG_RATIO_IN_MHZ_T = &
codata_constant("electron gyromag. ratio in MHz/T", &
real(28024.9514242d0,dp), real(0.0000085d0,dp), &
"MHz T^-1") !! electron gyromag. ratio in MHz/T

type(codata_constant), parameter, public :: ELECTRON_HELION_MASS_RATIO = &
codata_constant("electron-helion mass ratio", &
real(1.819543074573d-4,dp), real(0.000000000079d-4,dp), &
"") !! electron-helion mass ratio

type(codata_constant), parameter, public :: ELECTRON_MAG_MOM = &
codata_constant("electron mag. mom.", &
real(-9.2847647043d-24,dp), real(0.0000000028d-24,dp), &
"J T^-1") !! electron mag. mom.

type(codata_constant), parameter, public :: ELECTRON_MAG_MOM_ANOMALY = &
codata_constant("electron mag. mom. anomaly", &
real(1.15965218128d-3,dp), real(0.00000000018d-3,dp), &
"") !! electron mag. mom. anomaly

type(codata_constant), parameter, public :: ELECTRON_MAG_MOM_TO_BOHR_MAGNETON_RATIO = &
codata_constant("electron mag. mom. to Bohr magneton ratio", &
real(-1.00115965218128d0,dp), real(0.00000000000018d0,dp), &
"") !! electron mag. mom. to Bohr magneton ratio

type(codata_constant), parameter, public :: ELECTRON_MAG_MOM_TO_NUCLEAR_MAGNETON_RATIO = &
codata_constant("electron mag. mom. to nuclear magneton ratio", &
real(-1838.28197188d0,dp), real(0.00000011d0,dp), &
"") !! electron mag. mom. to nuclear magneton ratio

type(codata_constant), parameter, public :: ELECTRON_MASS = &
codata_constant("electron mass", &
real(9.1093837015d-31,dp), real(0.0000000028d-31,dp), &
"kg") !! electron mass

type(codata_constant), parameter, public :: ELECTRON_MASS_ENERGY_EQUIVALENT = &
codata_constant("electron mass energy equivalent", &
real(8.1871057769d-14,dp), real(0.0000000025d-14,dp), &
"J") !! electron mass energy equivalent

type(codata_constant), parameter, public :: ELECTRON_MASS_ENERGY_EQUIVALENT_IN_MEV = &
codata_constant("electron mass energy equivalent in MeV", &
real(0.51099895000d0,dp), real(0.00000000015d0,dp), &
"MeV") !! electron mass energy equivalent in MeV

type(codata_constant), parameter, public :: ELECTRON_MASS_IN_U = &
codata_constant("electron mass in u", &
real(5.48579909065d-4,dp), real(0.00000000016d-4,dp), &
"u") !! electron mass in u

type(codata_constant), parameter, public :: ELECTRON_MOLAR_MASS = &
codata_constant("electron molar mass", &
real(5.4857990888d-7,dp), real(0.0000000017d-7,dp), &
"kg mol^-1") !! electron molar mass

type(codata_constant), parameter, public :: ELECTRON_MUON_MAG_MOM_RATIO = &
codata_constant("electron-muon mag. mom. ratio", &
real(206.7669883d0,dp), real(0.0000046d0,dp), &
"") !! electron-muon mag. mom. ratio

type(codata_constant), parameter, public :: ELECTRON_MUON_MASS_RATIO = &
codata_constant("electron-muon mass ratio", &
real(4.83633169d-3,dp), real(0.00000011d-3,dp), &
"") !! electron-muon mass ratio

type(codata_constant), parameter, public :: ELECTRON_NEUTRON_MAG_MOM_RATIO = &
codata_constant("electron-neutron mag. mom. ratio", &
real(960.92050d0,dp), real(0.00023d0,dp), &
"") !! electron-neutron mag. mom. ratio

type(codata_constant), parameter, public :: ELECTRON_NEUTRON_MASS_RATIO = &
codata_constant("electron-neutron mass ratio", &
real(5.4386734424d-4,dp), real(0.0000000026d-4,dp), &
"") !! electron-neutron mass ratio

type(codata_constant), parameter, public :: ELECTRON_PROTON_MAG_MOM_RATIO = &
codata_constant("electron-proton mag. mom. ratio", &
real(-658.21068789d0,dp), real(0.00000020d0,dp), &
"") !! electron-proton mag. mom. ratio

type(codata_constant), parameter, public :: ELECTRON_PROTON_MASS_RATIO = &
codata_constant("electron-proton mass ratio", &
real(5.44617021487d-4,dp), real(0.00000000033d-4,dp), &
"") !! electron-proton mass ratio

type(codata_constant), parameter, public :: ELECTRON_RELATIVE_ATOMIC_MASS = &
codata_constant("electron relative atomic mass", &
real(5.48579909065d-4,dp), real(0.00000000016d-4,dp), &
"") !! electron relative atomic mass

type(codata_constant), parameter, public :: ELECTRON_TAU_MASS_RATIO = &
codata_constant("electron-tau mass ratio", &
real(2.87585d-4,dp), real(0.00019d-4,dp), &
"") !! electron-tau mass ratio

type(codata_constant), parameter, public :: ELECTRON_TO_ALPHA_PARTICLE_MASS_RATIO = &
codata_constant("electron to alpha particle mass ratio", &
real(1.370933554787d-4,dp), real(0.000000000045d-4,dp), &
"") !! electron to alpha particle mass ratio

type(codata_constant), parameter, public :: ELECTRON_TO_SHIELDED_HELION_MAG_MOM_RATIO = &
codata_constant("electron to shielded helion mag. mom. ratio", &
real(864.058257d0,dp), real(0.000010d0,dp), &
"") !! electron to shielded helion mag. mom. ratio

type(codata_constant), parameter, public :: ELECTRON_TO_SHIELDED_PROTON_MAG_MOM_RATIO = &
codata_constant("electron to shielded proton mag. mom. ratio", &
real(-658.2275971d0,dp), real(0.0000072d0,dp), &
"") !! electron to shielded proton mag. mom. ratio

type(codata_constant), parameter, public :: ELECTRON_TRITON_MASS_RATIO = &
codata_constant("electron-triton mass ratio", &
real(1.819200062251d-4,dp), real(0.000000000090d-4,dp), &
"") !! electron-triton mass ratio

type(codata_constant), parameter, public :: ELECTRON_VOLT = &
codata_constant("electron volt", &
real(1.602176634d-19,dp), real(0.0d0,dp), &
"J") !! electron volt

type(codata_constant), parameter, public :: ELECTRON_VOLT_ATOMIC_MASS_UNIT_RELATIONSHIP = &
codata_constant("electron volt-atomic mass unit relationship", &
real(1.07354410233d-9,dp), real(0.00000000032d-9,dp), &
"u") !! electron volt-atomic mass unit relationship

type(codata_constant), parameter, public :: ELECTRON_VOLT_HARTREE_RELATIONSHIP = &
codata_constant("electron volt-hartree relationship", &
real(3.6749322175655d-2,dp), real(0.0000000000071d-2,dp), &
"E_h") !! electron volt-hartree relationship

type(codata_constant), parameter, public :: ELECTRON_VOLT_HERTZ_RELATIONSHIP = &
codata_constant("electron volt-hertz relationship", &
real(2.417989242d14,dp), real(0.0d0,dp), &
"Hz") !! electron volt-hertz relationship

type(codata_constant), parameter, public :: ELECTRON_VOLT_INVERSE_METER_RELATIONSHIP = &
codata_constant("electron volt-inverse meter relationship", &
real(8.065543937d5,dp), real(0.0d0,dp), &
"m^-1") !! electron volt-inverse meter relationship

type(codata_constant), parameter, public :: ELECTRON_VOLT_JOULE_RELATIONSHIP = &
codata_constant("electron volt-joule relationship", &
real(1.602176634d-19,dp), real(0.0d0,dp), &
"J") !! electron volt-joule relationship

type(codata_constant), parameter, public :: ELECTRON_VOLT_KELVIN_RELATIONSHIP = &
codata_constant("electron volt-kelvin relationship", &
real(1.160451812d4,dp), real(0.0d0,dp), &
"K") !! electron volt-kelvin relationship

type(codata_constant), parameter, public :: ELECTRON_VOLT_KILOGRAM_RELATIONSHIP = &
codata_constant("electron volt-kilogram relationship", &
real(1.782661921d-36,dp), real(0.0d0,dp), &
"kg") !! electron volt-kilogram relationship

type(codata_constant), parameter, public :: ELEMENTARY_CHARGE = &
codata_constant("elementary charge", &
real(1.602176634d-19,dp), real(0.0d0,dp), &
"C") !! elementary charge

type(codata_constant), parameter, public :: ELEMENTARY_CHARGE_OVER_H_BAR = &
codata_constant("elementary charge over h-bar", &
real(1.519267447d15,dp), real(0.0d0,dp), &
"A J^-1") !! elementary charge over h-bar

type(codata_constant), parameter, public :: FARADAY_CONSTANT = &
codata_constant("Faraday constant", &
real(96485.33212d0,dp), real(0.0d0,dp), &
"C mol^-1") !! Faraday constant

type(codata_constant), parameter, public :: FERMI_COUPLING_CONSTANT = &
codata_constant("Fermi coupling constant", &
real(1.1663787d-5,dp), real(0.0000006d-5,dp), &
"GeV^-2") !! Fermi coupling constant

type(codata_constant), parameter, public :: FINE_STRUCTURE_CONSTANT = &
codata_constant("fine-structure constant", &
real(7.2973525693d-3,dp), real(0.0000000011d-3,dp), &
"") !! fine-structure constant

type(codata_constant), parameter, public :: FIRST_RADIATION_CONSTANT = &
codata_constant("first radiation constant", &
real(3.741771852d-16,dp), real(0.0d0,dp), &
"W m^2") !! first radiation constant

type(codata_constant), parameter, public :: FIRST_RADIATION_CONSTANT_FOR_SPECTRAL_RADIANCE = &
codata_constant("first radiation constant for spectral radiance", &
real(1.191042972d-16,dp), real(0.0d0,dp), &
"W m^2 sr^-1") !! first radiation constant for spectral radiance

type(codata_constant), parameter, public :: HARTREE_ATOMIC_MASS_UNIT_RELATIONSHIP = &
codata_constant("hartree-atomic mass unit relationship", &
real(2.92126232205d-8,dp), real(0.00000000088d-8,dp), &
"u") !! hartree-atomic mass unit relationship

type(codata_constant), parameter, public :: HARTREE_ELECTRON_VOLT_RELATIONSHIP = &
codata_constant("hartree-electron volt relationship", &
real(27.211386245988d0,dp), real(0.000000000053d0,dp), &
"eV") !! hartree-electron volt relationship

type(codata_constant), parameter, public :: HARTREE_ENERGY = &
codata_constant("Hartree energy", &
real(4.3597447222071d-18,dp), real(0.0000000000085d-18,dp), &
"J") !! Hartree energy

type(codata_constant), parameter, public :: HARTREE_ENERGY_IN_EV = &
codata_constant("Hartree energy in eV", &
real(27.211386245988d0,dp), real(0.000000000053d0,dp), &
"eV") !! Hartree energy in eV

type(codata_constant), parameter, public :: HARTREE_HERTZ_RELATIONSHIP = &
codata_constant("hartree-hertz relationship", &
real(6.579683920502d15,dp), real(0.000000000013d15,dp), &
"Hz") !! hartree-hertz relationship

type(codata_constant), parameter, public :: HARTREE_INVERSE_METER_RELATIONSHIP = &
codata_constant("hartree-inverse meter relationship", &
real(2.1947463136320d7,dp), real(0.0000000000043d7,dp), &
"m^-1") !! hartree-inverse meter relationship

type(codata_constant), parameter, public :: HARTREE_JOULE_RELATIONSHIP = &
codata_constant("hartree-joule relationship", &
real(4.3597447222071d-18,dp), real(0.0000000000085d-18,dp), &
"J") !! hartree-joule relationship

type(codata_constant), parameter, public :: HARTREE_KELVIN_RELATIONSHIP = &
codata_constant("hartree-kelvin relationship", &
real(3.1577502480407d5,dp), real(0.0000000000061d5,dp), &
"K") !! hartree-kelvin relationship

type(codata_constant), parameter, public :: HARTREE_KILOGRAM_RELATIONSHIP = &
codata_constant("hartree-kilogram relationship", &
real(4.8508702095432d-35,dp), real(0.0000000000094d-35,dp), &
"kg") !! hartree-kilogram relationship

type(codata_constant), parameter, public :: HELION_ELECTRON_MASS_RATIO = &
codata_constant("helion-electron mass ratio", &
real(5495.88528007d0,dp), real(0.00000024d0,dp), &
"") !! helion-electron mass ratio

type(codata_constant), parameter, public :: HELION_G_FACTOR = &
codata_constant("helion g factor", &
real(-4.255250615d0,dp), real(0.000000050d0,dp), &
"") !! helion g factor

type(codata_constant), parameter, public :: HELION_MAG_MOM = &
codata_constant("helion mag. mom.", &
real(-1.074617532d-26,dp), real(0.000000013d-26,dp), &
"J T^-1") !! helion mag. mom.

type(codata_constant), parameter, public :: HELION_MAG_MOM_TO_BOHR_MAGNETON_RATIO = &
codata_constant("helion mag. mom. to Bohr magneton ratio", &
real(-1.158740958d-3,dp), real(0.000000014d-3,dp), &
"") !! helion mag. mom. to Bohr magneton ratio

type(codata_constant), parameter, public :: HELION_MAG_MOM_TO_NUCLEAR_MAGNETON_RATIO = &
codata_constant("helion mag. mom. to nuclear magneton ratio", &
real(-2.127625307d0,dp), real(0.000000025d0,dp), &
"") !! helion mag. mom. to nuclear magneton ratio

type(codata_constant), parameter, public :: HELION_MASS = &
codata_constant("helion mass", &
real(5.0064127796d-27,dp), real(0.0000000015d-27,dp), &
"kg") !! helion mass

type(codata_constant), parameter, public :: HELION_MASS_ENERGY_EQUIVALENT = &
codata_constant("helion mass energy equivalent", &
real(4.4995394125d-10,dp), real(0.0000000014d-10,dp), &
"J") !! helion mass energy equivalent

type(codata_constant), parameter, public :: HELION_MASS_ENERGY_EQUIVALENT_IN_MEV = &
codata_constant("helion mass energy equivalent in MeV", &
real(2808.39160743d0,dp), real(0.00000085d0,dp), &
"MeV") !! helion mass energy equivalent in MeV

type(codata_constant), parameter, public :: HELION_MASS_IN_U = &
codata_constant("helion mass in u", &
real(3.014932247175d0,dp), real(0.000000000097d0,dp), &
"u") !! helion mass in u

type(codata_constant), parameter, public :: HELION_MOLAR_MASS = &
codata_constant("helion molar mass", &
real(3.01493224613d-3,dp), real(0.00000000091d-3,dp), &
"kg mol^-1") !! helion molar mass

type(codata_constant), parameter, public :: HELION_PROTON_MASS_RATIO = &
codata_constant("helion-proton mass ratio", &
real(2.99315267167d0,dp), real(0.00000000013d0,dp), &
"") !! helion-proton mass ratio

type(codata_constant), parameter, public :: HELION_RELATIVE_ATOMIC_MASS = &
codata_constant("helion relative atomic mass", &
real(3.014932247175d0,dp), real(0.000000000097d0,dp), &
"") !! helion relative atomic mass

type(codata_constant), parameter, public :: HELION_SHIELDING_SHIFT = &
codata_constant("helion shielding shift", &
real(5.996743d-5,dp), real(0.000010d-5,dp), &
"") !! helion shielding shift

type(codata_constant), parameter, public :: HERTZ_ATOMIC_MASS_UNIT_RELATIONSHIP = &
codata_constant("hertz-atomic mass unit relationship", &
real(4.4398216652d-24,dp), real(0.0000000013d-24,dp), &
"u") !! hertz-atomic mass unit relationship

type(codata_constant), parameter, public :: HERTZ_ELECTRON_VOLT_RELATIONSHIP = &
codata_constant("hertz-electron volt relationship", &
real(4.135667696d-15,dp), real(0.0d0,dp), &
"eV") !! hertz-electron volt relationship

type(codata_constant), parameter, public :: HERTZ_HARTREE_RELATIONSHIP = &
codata_constant("hertz-hartree relationship", &
real(1.5198298460570d-16,dp), real(0.0000000000029d-16,dp), &
"E_h") !! hertz-hartree relationship

type(codata_constant), parameter, public :: HERTZ_INVERSE_METER_RELATIONSHIP = &
codata_constant("hertz-inverse meter relationship", &
real(3.335640951d-9,dp), real(0.0d0,dp), &
"m^-1") !! hertz-inverse meter relationship

type(codata_constant), parameter, public :: HERTZ_JOULE_RELATIONSHIP = &
codata_constant("hertz-joule relationship", &
real(6.62607015d-34,dp), real(0.0d0,dp), &
"J") !! hertz-joule relationship

type(codata_constant), parameter, public :: HERTZ_KELVIN_RELATIONSHIP = &
codata_constant("hertz-kelvin relationship", &
real(4.799243073d-11,dp), real(0.0d0,dp), &
"K") !! hertz-kelvin relationship

type(codata_constant), parameter, public :: HERTZ_KILOGRAM_RELATIONSHIP = &
codata_constant("hertz-kilogram relationship", &
real(7.372497323d-51,dp), real(0.0d0,dp), &
"kg") !! hertz-kilogram relationship

type(codata_constant), parameter, public :: HYPERFINE_TRANSITION_FREQUENCY_OF_CS_133 = &
codata_constant("hyperfine transition frequency of Cs-133", &
real(9192631770d0,dp), real(0.0d0,dp), &
"Hz") !! hyperfine transition frequency of Cs-133

type(codata_constant), parameter, public :: INVERSE_FINE_STRUCTURE_CONSTANT = &
codata_constant("inverse fine-structure constant", &
real(137.035999084d0,dp), real(0.000000021d0,dp), &
"") !! inverse fine-structure constant

type(codata_constant), parameter, public :: INVERSE_METER_ATOMIC_MASS_UNIT_RELATIONSHIP = &
codata_constant("inverse meter-atomic mass unit relationship", &
real(1.33102505010d-15,dp), real(0.00000000040d-15,dp), &
"u") !! inverse meter-atomic mass unit relationship

type(codata_constant), parameter, public :: INVERSE_METER_ELECTRON_VOLT_RELATIONSHIP = &
codata_constant("inverse meter-electron volt relationship", &
real(1.239841984d-6,dp), real(0.0d0,dp), &
"eV") !! inverse meter-electron volt relationship

type(codata_constant), parameter, public :: INVERSE_METER_HARTREE_RELATIONSHIP = &
codata_constant("inverse meter-hartree relationship", &
real(4.5563352529120d-8,dp), real(0.0000000000088d-8,dp), &
"E_h") !! inverse meter-hartree relationship

type(codata_constant), parameter, public :: INVERSE_METER_HERTZ_RELATIONSHIP = &
codata_constant("inverse meter-hertz relationship", &
real(299792458d0,dp), real(0.0d0,dp), &
"Hz") !! inverse meter-hertz relationship

type(codata_constant), parameter, public :: INVERSE_METER_JOULE_RELATIONSHIP = &
codata_constant("inverse meter-joule relationship", &
real(1.986445857d-25,dp), real(0.0d0,dp), &
"J") !! inverse meter-joule relationship

type(codata_constant), parameter, public :: INVERSE_METER_KELVIN_RELATIONSHIP = &
codata_constant("inverse meter-kelvin relationship", &
real(1.438776877d-2,dp), real(0.0d0,dp), &
"K") !! inverse meter-kelvin relationship

type(codata_constant), parameter, public :: INVERSE_METER_KILOGRAM_RELATIONSHIP = &
codata_constant("inverse meter-kilogram relationship", &
real(2.210219094d-42,dp), real(0.0d0,dp), &
"kg") !! inverse meter-kilogram relationship

type(codata_constant), parameter, public :: INVERSE_OF_CONDUCTANCE_QUANTUM = &
codata_constant("inverse of conductance quantum", &
real(12906.40372d0,dp), real(0.0d0,dp), &
"ohm") !! inverse of conductance quantum

type(codata_constant), parameter, public :: JOSEPHSON_CONSTANT = &
codata_constant("Josephson constant", &
real(483597.8484d9,dp), real(0.0d0,dp), &
"Hz V^-1") !! Josephson constant

type(codata_constant), parameter, public :: JOULE_ATOMIC_MASS_UNIT_RELATIONSHIP = &
codata_constant("joule-atomic mass unit relationship", &
real(6.7005352565d9,dp), real(0.0000000020d9,dp), &
"u") !! joule-atomic mass unit relationship

type(codata_constant), parameter, public :: JOULE_ELECTRON_VOLT_RELATIONSHIP = &
codata_constant("joule-electron volt relationship", &
real(6.241509074d18,dp), real(0.0d0,dp), &
"eV") !! joule-electron volt relationship

type(codata_constant), parameter, public :: JOULE_HARTREE_RELATIONSHIP = &
codata_constant("joule-hartree relationship", &
real(2.2937122783963d17,dp), real(0.0000000000045d17,dp), &
"E_h") !! joule-hartree relationship

type(codata_constant), parameter, public :: JOULE_HERTZ_RELATIONSHIP = &
codata_constant("joule-hertz relationship", &
real(1.509190179d33,dp), real(0.0d0,dp), &
"Hz") !! joule-hertz relationship

type(codata_constant), parameter, public :: JOULE_INVERSE_METER_RELATIONSHIP = &
codata_constant("joule-inverse meter relationship", &
real(5.034116567d24,dp), real(0.0d0,dp), &
"m^-1") !! joule-inverse meter relationship

type(codata_constant), parameter, public :: JOULE_KELVIN_RELATIONSHIP = &
codata_constant("joule-kelvin relationship", &
real(7.242970516d22,dp), real(0.0d0,dp), &
"K") !! joule-kelvin relationship

type(codata_constant), parameter, public :: JOULE_KILOGRAM_RELATIONSHIP = &
codata_constant("joule-kilogram relationship", &
real(1.112650056d-17,dp), real(0.0d0,dp), &
"kg") !! joule-kilogram relationship

type(codata_constant), parameter, public :: KELVIN_ATOMIC_MASS_UNIT_RELATIONSHIP = &
codata_constant("kelvin-atomic mass unit relationship", &
real(9.2510873014d-14,dp), real(0.0000000028d-14,dp), &
"u") !! kelvin-atomic mass unit relationship

type(codata_constant), parameter, public :: KELVIN_ELECTRON_VOLT_RELATIONSHIP = &
codata_constant("kelvin-electron volt relationship", &
real(8.617333262d-5,dp), real(0.0d0,dp), &
"eV") !! kelvin-electron volt relationship

type(codata_constant), parameter, public :: KELVIN_HARTREE_RELATIONSHIP = &
codata_constant("kelvin-hartree relationship", &
real(3.1668115634556d-6,dp), real(0.0000000000061d-6,dp), &
"E_h") !! kelvin-hartree relationship

type(codata_constant), parameter, public :: KELVIN_HERTZ_RELATIONSHIP = &
codata_constant("kelvin-hertz relationship", &
real(2.083661912d10,dp), real(0.0d0,dp), &
"Hz") !! kelvin-hertz relationship

type(codata_constant), parameter, public :: KELVIN_INVERSE_METER_RELATIONSHIP = &
codata_constant("kelvin-inverse meter relationship", &
real(69.50348004d0,dp), real(0.0d0,dp), &
"m^-1") !! kelvin-inverse meter relationship

type(codata_constant), parameter, public :: KELVIN_JOULE_RELATIONSHIP = &
codata_constant("kelvin-joule relationship", &
real(1.380649d-23,dp), real(0.0d0,dp), &
"J") !! kelvin-joule relationship

type(codata_constant), parameter, public :: KELVIN_KILOGRAM_RELATIONSHIP = &
codata_constant("kelvin-kilogram relationship", &
real(1.536179187d-40,dp), real(0.0d0,dp), &
"kg") !! kelvin-kilogram relationship

type(codata_constant), parameter, public :: KILOGRAM_ATOMIC_MASS_UNIT_RELATIONSHIP = &
codata_constant("kilogram-atomic mass unit relationship", &
real(6.0221407621d26,dp), real(0.0000000018d26,dp), &
"u") !! kilogram-atomic mass unit relationship

type(codata_constant), parameter, public :: KILOGRAM_ELECTRON_VOLT_RELATIONSHIP = &
codata_constant("kilogram-electron volt relationship", &
real(5.609588603d35,dp), real(0.0d0,dp), &
"eV") !! kilogram-electron volt relationship

type(codata_constant), parameter, public :: KILOGRAM_HARTREE_RELATIONSHIP = &
codata_constant("kilogram-hartree relationship", &
real(2.0614857887409d34,dp), real(0.0000000000040d34,dp), &
"E_h") !! kilogram-hartree relationship

type(codata_constant), parameter, public :: KILOGRAM_HERTZ_RELATIONSHIP = &
codata_constant("kilogram-hertz relationship", &
real(1.356392489d50,dp), real(0.0d0,dp), &
"Hz") !! kilogram-hertz relationship

type(codata_constant), parameter, public :: KILOGRAM_INVERSE_METER_RELATIONSHIP = &
codata_constant("kilogram-inverse meter relationship", &
real(4.524438335d41,dp), real(0.0d0,dp), &
"m^-1") !! kilogram-inverse meter relationship

type(codata_constant), parameter, public :: KILOGRAM_JOULE_RELATIONSHIP = &
codata_constant("kilogram-joule relationship", &
real(8.987551787d16,dp), real(0.0d0,dp), &
"J") !! kilogram-joule relationship

type(codata_constant), parameter, public :: KILOGRAM_KELVIN_RELATIONSHIP = &
codata_constant("kilogram-kelvin relationship", &
real(6.509657260d39,dp), real(0.0d0,dp), &
"K") !! kilogram-kelvin relationship

type(codata_constant), parameter, public :: LATTICE_PARAMETER_OF_SILICON = &
codata_constant("lattice parameter of silicon", &
real(5.431020511d-10,dp), real(0.000000089d-10,dp), &
"m") !! lattice parameter of silicon

type(codata_constant), parameter, public :: LATTICE_SPACING_OF_IDEAL_SI_220 = &
codata_constant("lattice spacing of ideal Si (220)", &
real(1.920155716d-10,dp), real(0.000000032d-10,dp), &
"m") !! lattice spacing of ideal Si (220)

type(codata_constant), parameter, public :: LOSCHMIDT_CONSTANT_27315_K_100_KPA = &
codata_constant("Loschmidt constant (273.15 K, 100 kPa)", &
real(2.651645804d25,dp), real(0.0d0,dp), &
"m^-3") !! Loschmidt constant (273.15 K, 100 kPa)

type(codata_constant), parameter, public :: LOSCHMIDT_CONSTANT_27315_K_101325_KPA = &
codata_constant("Loschmidt constant (273.15 K, 101.325 kPa)", &
real(2.686780111d25,dp), real(0.0d0,dp), &
"m^-3") !! Loschmidt constant (273.15 K, 101.325 kPa)

type(codata_constant), parameter, public :: LUMINOUS_EFFICACY = &
codata_constant("luminous efficacy", &
real(683d0,dp), real(0.0d0,dp), &
"lm W^-1") !! luminous efficacy

type(codata_constant), parameter, public :: MAG_FLUX_QUANTUM = &
codata_constant("mag. flux quantum", &
real(2.067833848d-15,dp), real(0.0d0,dp), &
"Wb") !! mag. flux quantum

type(codata_constant), parameter, public :: MOLAR_GAS_CONSTANT = &
codata_constant("molar gas constant", &
real(8.314462618d0,dp), real(0.0d0,dp), &
"J mol^-1 K^-1") !! molar gas constant

type(codata_constant), parameter, public :: MOLAR_MASS_CONSTANT = &
codata_constant("molar mass constant", &
real(0.99999999965d-3,dp), real(0.00000000030d-3,dp), &
"kg mol^-1") !! molar mass constant

type(codata_constant), parameter, public :: MOLAR_MASS_OF_CARBON_12 = &
codata_constant("molar mass of carbon-12", &
real(11.9999999958d-3,dp), real(0.0000000036d-3,dp), &
"kg mol^-1") !! molar mass of carbon-12

type(codata_constant), parameter, public :: MOLAR_PLANCK_CONSTANT = &
codata_constant("molar Planck constant", &
real(3.990312712d-10,dp), real(0.0d0,dp), &
"J Hz^-1 mol^-1") !! molar Planck constant

type(codata_constant), parameter, public :: MOLAR_VOLUME_OF_IDEAL_GAS_27315_K_100_KPA = &
codata_constant("molar volume of ideal gas (273.15 K, 100 kPa)", &
real(22.71095464d-3,dp), real(0.0d0,dp), &
"m^3 mol^-1") !! molar volume of ideal gas (273.15 K, 100 kPa)

type(codata_constant), parameter, public :: MOLAR_VOLUME_OF_IDEAL_GAS_27315_K_101325_KPA = &
codata_constant("molar volume of ideal gas (273.15 K, 101.325 kPa)", &
real(22.41396954d-3,dp), real(0.0d0,dp), &
"m^3 mol^-1") !! molar volume of ideal gas (273.15 K, 101.325 kPa)

type(codata_constant), parameter, public :: MOLAR_VOLUME_OF_SILICON = &
codata_constant("molar volume of silicon", &
real(1.205883199d-5,dp), real(0.000000060d-5,dp), &
"m^3 mol^-1") !! molar volume of silicon

type(codata_constant), parameter, public :: MOLYBDENUM_X_UNIT = &
codata_constant("Molybdenum x unit", &
real(1.00209952d-13,dp), real(0.00000053d-13,dp), &
"m") !! Molybdenum x unit

type(codata_constant), parameter, public :: MUON_COMPTON_WAVELENGTH = &
codata_constant("muon Compton wavelength", &
real(1.173444110d-14,dp), real(0.000000026d-14,dp), &
"m") !! muon Compton wavelength

type(codata_constant), parameter, public :: MUON_ELECTRON_MASS_RATIO = &
codata_constant("muon-electron mass ratio", &
real(206.7682830d0,dp), real(0.0000046d0,dp), &
"") !! muon-electron mass ratio

type(codata_constant), parameter, public :: MUON_G_FACTOR = &
codata_constant("muon g factor", &
real(-2.0023318418d0,dp), real(0.0000000013d0,dp), &
"") !! muon g factor

type(codata_constant), parameter, public :: MUON_MAG_MOM = &
codata_constant("muon mag. mom.", &
real(-4.49044830d-26,dp), real(0.00000010d-26,dp), &
"J T^-1") !! muon mag. mom.

type(codata_constant), parameter, public :: MUON_MAG_MOM_ANOMALY = &
codata_constant("muon mag. mom. anomaly", &
real(1.16592089d-3,dp), real(0.00000063d-3,dp), &
"") !! muon mag. mom. anomaly

type(codata_constant), parameter, public :: MUON_MAG_MOM_TO_BOHR_MAGNETON_RATIO = &
codata_constant("muon mag. mom. to Bohr magneton ratio", &
real(-4.84197047d-3,dp), real(0.00000011d-3,dp), &
"") !! muon mag. mom. to Bohr magneton ratio

type(codata_constant), parameter, public :: MUON_MAG_MOM_TO_NUCLEAR_MAGNETON_RATIO = &
codata_constant("muon mag. mom. to nuclear magneton ratio", &
real(-8.89059703d0,dp), real(0.00000020d0,dp), &
"") !! muon mag. mom. to nuclear magneton ratio

type(codata_constant), parameter, public :: MUON_MASS = &
codata_constant("muon mass", &
real(1.883531627d-28,dp), real(0.000000042d-28,dp), &
"kg") !! muon mass

type(codata_constant), parameter, public :: MUON_MASS_ENERGY_EQUIVALENT = &
codata_constant("muon mass energy equivalent", &
real(1.692833804d-11,dp), real(0.000000038d-11,dp), &
"J") !! muon mass energy equivalent

type(codata_constant), parameter, public :: MUON_MASS_ENERGY_EQUIVALENT_IN_MEV = &
codata_constant("muon mass energy equivalent in MeV", &
real(105.6583755d0,dp), real(0.0000023d0,dp), &
"MeV") !! muon mass energy equivalent in MeV

type(codata_constant), parameter, public :: MUON_MASS_IN_U = &
codata_constant("muon mass in u", &
real(0.1134289259d0,dp), real(0.0000000025d0,dp), &
"u") !! muon mass in u

type(codata_constant), parameter, public :: MUON_MOLAR_MASS = &
codata_constant("muon molar mass", &
real(1.134289259d-4,dp), real(0.000000025d-4,dp), &
"kg mol^-1") !! muon molar mass

type(codata_constant), parameter, public :: MUON_NEUTRON_MASS_RATIO = &
codata_constant("muon-neutron mass ratio", &
real(0.1124545170d0,dp), real(0.0000000025d0,dp), &
"") !! muon-neutron mass ratio

type(codata_constant), parameter, public :: MUON_PROTON_MAG_MOM_RATIO = &
codata_constant("muon-proton mag. mom. ratio", &
real(-3.183345142d0,dp), real(0.000000071d0,dp), &
"") !! muon-proton mag. mom. ratio

type(codata_constant), parameter, public :: MUON_PROTON_MASS_RATIO = &
codata_constant("muon-proton mass ratio", &
real(0.1126095264d0,dp), real(0.0000000025d0,dp), &
"") !! muon-proton mass ratio

type(codata_constant), parameter, public :: MUON_TAU_MASS_RATIO = &
codata_constant("muon-tau mass ratio", &
real(5.94635d-2,dp), real(0.00040d-2,dp), &
"") !! muon-tau mass ratio

type(codata_constant), parameter, public :: NATURAL_UNIT_OF_ACTION = &
codata_constant("natural unit of action", &
real(1.054571817d-34,dp), real(0.0d0,dp), &
"J s") !! natural unit of action

type(codata_constant), parameter, public :: NATURAL_UNIT_OF_ACTION_IN_EV_S = &
codata_constant("natural unit of action in eV s", &
real(6.582119569d-16,dp), real(0.0d0,dp), &
"eV s") !! natural unit of action in eV s

type(codata_constant), parameter, public :: NATURAL_UNIT_OF_ENERGY = &
codata_constant("natural unit of energy", &
real(8.1871057769d-14,dp), real(0.0000000025d-14,dp), &
"J") !! natural unit of energy

type(codata_constant), parameter, public :: NATURAL_UNIT_OF_ENERGY_IN_MEV = &
codata_constant("natural unit of energy in MeV", &
real(0.51099895000d0,dp), real(0.00000000015d0,dp), &
"MeV") !! natural unit of energy in MeV

type(codata_constant), parameter, public :: NATURAL_UNIT_OF_LENGTH = &
codata_constant("natural unit of length", &
real(3.8615926796d-13,dp), real(0.0000000012d-13,dp), &
"m") !! natural unit of length

type(codata_constant), parameter, public :: NATURAL_UNIT_OF_MASS = &
codata_constant("natural unit of mass", &
real(9.1093837015d-31,dp), real(0.0000000028d-31,dp), &
"kg") !! natural unit of mass

type(codata_constant), parameter, public :: NATURAL_UNIT_OF_MOMENTUM = &
codata_constant("natural unit of momentum", &
real(2.73092453075d-22,dp), real(0.00000000082d-22,dp), &
"kg m s^-1") !! natural unit of momentum

type(codata_constant), parameter, public :: NATURAL_UNIT_OF_MOMENTUM_IN_MEV_C = &
codata_constant("natural unit of momentum in MeV/c", &
real(0.51099895000d0,dp), real(0.00000000015d0,dp), &
"MeV/c") !! natural unit of momentum in MeV/c

type(codata_constant), parameter, public :: NATURAL_UNIT_OF_TIME = &
codata_constant("natural unit of time", &
real(1.28808866819d-21,dp), real(0.00000000039d-21,dp), &
"s") !! natural unit of time

type(codata_constant), parameter, public :: NATURAL_UNIT_OF_VELOCITY = &
codata_constant("natural unit of velocity", &
real(299792458d0,dp), real(0.0d0,dp), &
"m s^-1") !! natural unit of velocity

type(codata_constant), parameter, public :: NEUTRON_COMPTON_WAVELENGTH = &
codata_constant("neutron Compton wavelength", &
real(1.31959090581d-15,dp), real(0.00000000075d-15,dp), &
"m") !! neutron Compton wavelength

type(codata_constant), parameter, public :: NEUTRON_ELECTRON_MAG_MOM_RATIO = &
codata_constant("neutron-electron mag. mom. ratio", &
real(1.04066882d-3,dp), real(0.00000025d-3,dp), &
"") !! neutron-electron mag. mom. ratio

type(codata_constant), parameter, public :: NEUTRON_ELECTRON_MASS_RATIO = &
codata_constant("neutron-electron mass ratio", &
real(1838.68366173d0,dp), real(0.00000089d0,dp), &
"") !! neutron-electron mass ratio

type(codata_constant), parameter, public :: NEUTRON_G_FACTOR = &
codata_constant("neutron g factor", &
real(-3.82608545d0,dp), real(0.00000090d0,dp), &
"") !! neutron g factor

type(codata_constant), parameter, public :: NEUTRON_GYROMAG_RATIO = &
codata_constant("neutron gyromag. ratio", &
real(1.83247171d8,dp), real(0.00000043d8,dp), &
"s^-1 T^-1") !! neutron gyromag. ratio

type(codata_constant), parameter, public :: NEUTRON_GYROMAG_RATIO_IN_MHZ_T = &
codata_constant("neutron gyromag. ratio in MHz/T", &
real(29.1646931d0,dp), real(0.0000069d0,dp), &
"MHz T^-1") !! neutron gyromag. ratio in MHz/T

type(codata_constant), parameter, public :: NEUTRON_MAG_MOM = &
codata_constant("neutron mag. mom.", &
real(-9.6623651d-27,dp), real(0.0000023d-27,dp), &
"J T^-1") !! neutron mag. mom.

type(codata_constant), parameter, public :: NEUTRON_MAG_MOM_TO_BOHR_MAGNETON_RATIO = &
codata_constant("neutron mag. mom. to Bohr magneton ratio", &
real(-1.04187563d-3,dp), real(0.00000025d-3,dp), &
"") !! neutron mag. mom. to Bohr magneton ratio

type(codata_constant), parameter, public :: NEUTRON_MAG_MOM_TO_NUCLEAR_MAGNETON_RATIO = &
codata_constant("neutron mag. mom. to nuclear magneton ratio", &
real(-1.91304273d0,dp), real(0.00000045d0,dp), &
"") !! neutron mag. mom. to nuclear magneton ratio

type(codata_constant), parameter, public :: NEUTRON_MASS = &
codata_constant("neutron mass", &
real(1.67492749804d-27,dp), real(0.00000000095d-27,dp), &
"kg") !! neutron mass

type(codata_constant), parameter, public :: NEUTRON_MASS_ENERGY_EQUIVALENT = &
codata_constant("neutron mass energy equivalent", &
real(1.50534976287d-10,dp), real(0.00000000086d-10,dp), &
"J") !! neutron mass energy equivalent

type(codata_constant), parameter, public :: NEUTRON_MASS_ENERGY_EQUIVALENT_IN_MEV = &
codata_constant("neutron mass energy equivalent in MeV", &
real(939.56542052d0,dp), real(0.00000054d0,dp), &
"MeV") !! neutron mass energy equivalent in MeV

type(codata_constant), parameter, public :: NEUTRON_MASS_IN_U = &
codata_constant("neutron mass in u", &
real(1.00866491595d0,dp), real(0.00000000049d0,dp), &
"u") !! neutron mass in u

type(codata_constant), parameter, public :: NEUTRON_MOLAR_MASS = &
codata_constant("neutron molar mass", &
real(1.00866491560d-3,dp), real(0.00000000057d-3,dp), &
"kg mol^-1") !! neutron molar mass

type(codata_constant), parameter, public :: NEUTRON_MUON_MASS_RATIO = &
codata_constant("neutron-muon mass ratio", &
real(8.89248406d0,dp), real(0.00000020d0,dp), &
"") !! neutron-muon mass ratio

type(codata_constant), parameter, public :: NEUTRON_PROTON_MAG_MOM_RATIO = &
codata_constant("neutron-proton mag. mom. ratio", &
real(-0.68497934d0,dp), real(0.00000016d0,dp), &
"") !! neutron-proton mag. mom. ratio

type(codata_constant), parameter, public :: NEUTRON_PROTON_MASS_DIFFERENCE = &
codata_constant("neutron-proton mass difference", &
real(2.30557435d-30,dp), real(0.00000082d-30,dp), &
"kg") !! neutron-proton mass difference

type(codata_constant), parameter, public :: NEUTRON_PROTON_MASS_DIFFERENCE_ENERGY_EQUIVALENT = &
codata_constant("neutron-proton mass difference energy equivalent", &
real(2.07214689d-13,dp), real(0.00000074d-13,dp), &
"J") !! neutron-proton mass difference energy equivalent

type(codata_constant), parameter, public :: NEUTRON_PROTON_MASS_DIFFERENCE_ENERGY_EQUIVALENT_IN_MEV = &
codata_constant("neutron-proton mass difference energy equivalent in MeV", &
real(1.29333236d0,dp), real(0.00000046d0,dp), &
"MeV") !! neutron-proton mass difference energy equivalent in MeV

type(codata_constant), parameter, public :: NEUTRON_PROTON_MASS_DIFFERENCE_IN_U = &
codata_constant("neutron-proton mass difference in u", &
real(1.38844933d-3,dp), real(0.00000049d-3,dp), &
"u") !! neutron-proton mass difference in u

type(codata_constant), parameter, public :: NEUTRON_PROTON_MASS_RATIO = &
codata_constant("neutron-proton mass ratio", &
real(1.00137841931d0,dp), real(0.00000000049d0,dp), &
"") !! neutron-proton mass ratio

type(codata_constant), parameter, public :: NEUTRON_RELATIVE_ATOMIC_MASS = &
codata_constant("neutron relative atomic mass", &
real(1.00866491595d0,dp), real(0.00000000049d0,dp), &
"") !! neutron relative atomic mass

type(codata_constant), parameter, public :: NEUTRON_TAU_MASS_RATIO = &
codata_constant("neutron-tau mass ratio", &
real(0.528779d0,dp), real(0.000036d0,dp), &
"") !! neutron-tau mass ratio

type(codata_constant), parameter, public :: NEUTRON_TO_SHIELDED_PROTON_MAG_MOM_RATIO = &
codata_constant("neutron to shielded proton mag. mom. ratio", &
real(-0.68499694d0,dp), real(0.00000016d0,dp), &
"") !! neutron to shielded proton mag. mom. ratio

type(codata_constant), parameter, public :: NEWTONIAN_CONSTANT_OF_GRAVITATION = &
codata_constant("Newtonian constant of gravitation", &
real(6.67430d-11,dp), real(0.00015d-11,dp), &
"m^3 kg^-1 s^-2") !! Newtonian constant of gravitation

type(codata_constant), parameter, public :: NEWTONIAN_CONSTANT_OF_GRAVITATION_OVER_H_BAR_C = &
codata_constant("Newtonian constant of gravitation over h-bar c", &
real(6.70883d-39,dp), real(0.00015d-39,dp), &
"(GeV/c^2)^-2") !! Newtonian constant of gravitation over h-bar c

type(codata_constant), parameter, public :: NUCLEAR_MAGNETON = &
codata_constant("nuclear magneton", &
real(5.0507837461d-27,dp), real(0.0000000015d-27,dp), &
"J T^-1") !! nuclear magneton

type(codata_constant), parameter, public :: NUCLEAR_MAGNETON_IN_EV_T = &
codata_constant("nuclear magneton in eV/T", &
real(3.15245125844d-8,dp), real(0.00000000096d-8,dp), &
"eV T^-1") !! nuclear magneton in eV/T

type(codata_constant), parameter, public :: NUCLEAR_MAGNETON_IN_INVERSE_METER_PER_TESLA = &
codata_constant("nuclear magneton in inverse meter per tesla", &
real(2.54262341353d-2,dp), real(0.00000000078d-2,dp), &
"m^-1 T^-1") !! nuclear magneton in inverse meter per tesla

type(codata_constant), parameter, public :: NUCLEAR_MAGNETON_IN_K_T = &
codata_constant("nuclear magneton in K/T", &
real(3.6582677756d-4,dp), real(0.0000000011d-4,dp), &
"K T^-1") !! nuclear magneton in K/T

type(codata_constant), parameter, public :: NUCLEAR_MAGNETON_IN_MHZ_T = &
codata_constant("nuclear magneton in MHz/T", &
real(7.6225932291d0,dp), real(0.0000000023d0,dp), &
"MHz T^-1") !! nuclear magneton in MHz/T

type(codata_constant), parameter, public :: PLANCK_CONSTANT = &
codata_constant("Planck constant", &
real(6.62607015d-34,dp), real(0.0d0,dp), &
"J Hz^-1") !! Planck constant

type(codata_constant), parameter, public :: PLANCK_CONSTANT_IN_EV_HZ = &
codata_constant("Planck constant in eV/Hz", &
real(4.135667696d-15,dp), real(0.0d0,dp), &
"eV Hz^-1") !! Planck constant in eV/Hz

type(codata_constant), parameter, public :: PLANCK_LENGTH = &
codata_constant("Planck length", &
real(1.616255d-35,dp), real(0.000018d-35,dp), &
"m") !! Planck length

type(codata_constant), parameter, public :: PLANCK_MASS = &
codata_constant("Planck mass", &
real(2.176434d-8,dp), real(0.000024d-8,dp), &
"kg") !! Planck mass

type(codata_constant), parameter, public :: PLANCK_MASS_ENERGY_EQUIVALENT_IN_GEV = &
codata_constant("Planck mass energy equivalent in GeV", &
real(1.220890d19,dp), real(0.000014d19,dp), &
"GeV") !! Planck mass energy equivalent in GeV

type(codata_constant), parameter, public :: PLANCK_TEMPERATURE = &
codata_constant("Planck temperature", &
real(1.416784d32,dp), real(0.000016d32,dp), &
"K") !! Planck temperature

type(codata_constant), parameter, public :: PLANCK_TIME = &
codata_constant("Planck time", &
real(5.391247d-44,dp), real(0.000060d-44,dp), &
"s") !! Planck time

type(codata_constant), parameter, public :: PROTON_CHARGE_TO_MASS_QUOTIENT = &
codata_constant("proton charge to mass quotient", &
real(9.5788331560d7,dp), real(0.0000000029d7,dp), &
"C kg^-1") !! proton charge to mass quotient

type(codata_constant), parameter, public :: PROTON_COMPTON_WAVELENGTH = &
codata_constant("proton Compton wavelength", &
real(1.32140985539d-15,dp), real(0.00000000040d-15,dp), &
"m") !! proton Compton wavelength

type(codata_constant), parameter, public :: PROTON_ELECTRON_MASS_RATIO = &
codata_constant("proton-electron mass ratio", &
real(1836.15267343d0,dp), real(0.00000011d0,dp), &
"") !! proton-electron mass ratio

type(codata_constant), parameter, public :: PROTON_G_FACTOR = &
codata_constant("proton g factor", &
real(5.5856946893d0,dp), real(0.0000000016d0,dp), &
"") !! proton g factor

type(codata_constant), parameter, public :: PROTON_GYROMAG_RATIO = &
codata_constant("proton gyromag. ratio", &
real(2.6752218744d8,dp), real(0.0000000011d8,dp), &
"s^-1 T^-1") !! proton gyromag. ratio

type(codata_constant), parameter, public :: PROTON_GYROMAG_RATIO_IN_MHZ_T = &
codata_constant("proton gyromag. ratio in MHz/T", &
real(42.577478518d0,dp), real(0.000000018d0,dp), &
"MHz T^-1") !! proton gyromag. ratio in MHz/T

type(codata_constant), parameter, public :: PROTON_MAG_MOM = &
codata_constant("proton mag. mom.", &
real(1.41060679736d-26,dp), real(0.00000000060d-26,dp), &
"J T^-1") !! proton mag. mom.

type(codata_constant), parameter, public :: PROTON_MAG_MOM_TO_BOHR_MAGNETON_RATIO = &
codata_constant("proton mag. mom. to Bohr magneton ratio", &
real(1.52103220230d-3,dp), real(0.00000000046d-3,dp), &
"") !! proton mag. mom. to Bohr magneton ratio

type(codata_constant), parameter, public :: PROTON_MAG_MOM_TO_NUCLEAR_MAGNETON_RATIO = &
codata_constant("proton mag. mom. to nuclear magneton ratio", &
real(2.79284734463d0,dp), real(0.00000000082d0,dp), &
"") !! proton mag. mom. to nuclear magneton ratio

type(codata_constant), parameter, public :: PROTON_MAG_SHIELDING_CORRECTION = &
codata_constant("proton mag. shielding correction", &
real(2.5689d-5,dp), real(0.0011d-5,dp), &
"") !! proton mag. shielding correction

type(codata_constant), parameter, public :: PROTON_MASS = &
codata_constant("proton mass", &
real(1.67262192369d-27,dp), real(0.00000000051d-27,dp), &
"kg") !! proton mass

type(codata_constant), parameter, public :: PROTON_MASS_ENERGY_EQUIVALENT = &
codata_constant("proton mass energy equivalent", &
real(1.50327761598d-10,dp), real(0.00000000046d-10,dp), &
"J") !! proton mass energy equivalent

type(codata_constant), parameter, public :: PROTON_MASS_ENERGY_EQUIVALENT_IN_MEV = &
codata_constant("proton mass energy equivalent in MeV", &
real(938.27208816d0,dp), real(0.00000029d0,dp), &
"MeV") !! proton mass energy equivalent in MeV

type(codata_constant), parameter, public :: PROTON_MASS_IN_U = &
codata_constant("proton mass in u", &
real(1.007276466621d0,dp), real(0.000000000053d0,dp), &
"u") !! proton mass in u

type(codata_constant), parameter, public :: PROTON_MOLAR_MASS = &
codata_constant("proton molar mass", &
real(1.00727646627d-3,dp), real(0.00000000031d-3,dp), &
"kg mol^-1") !! proton molar mass

type(codata_constant), parameter, public :: PROTON_MUON_MASS_RATIO = &
codata_constant("proton-muon mass ratio", &
real(8.88024337d0,dp), real(0.00000020d0,dp), &
"") !! proton-muon mass ratio

type(codata_constant), parameter, public :: PROTON_NEUTRON_MAG_MOM_RATIO = &
codata_constant("proton-neutron mag. mom. ratio", &
real(-1.45989805d0,dp), real(0.00000034d0,dp), &
"") !! proton-neutron mag. mom. ratio

type(codata_constant), parameter, public :: PROTON_NEUTRON_MASS_RATIO = &
codata_constant("proton-neutron mass ratio", &
real(0.99862347812d0,dp), real(0.00000000049d0,dp), &
"") !! proton-neutron mass ratio

type(codata_constant), parameter, public :: PROTON_RELATIVE_ATOMIC_MASS = &
codata_constant("proton relative atomic mass", &
real(1.007276466621d0,dp), real(0.000000000053d0,dp), &
"") !! proton relative atomic mass

type(codata_constant), parameter, public :: PROTON_RMS_CHARGE_RADIUS = &
codata_constant("proton rms charge radius", &
real(8.414d-16,dp), real(0.019d-16,dp), &
"m") !! proton rms charge radius

type(codata_constant), parameter, public :: PROTON_TAU_MASS_RATIO = &
codata_constant("proton-tau mass ratio", &
real(0.528051d0,dp), real(0.000036d0,dp), &
"") !! proton-tau mass ratio

type(codata_constant), parameter, public :: QUANTUM_OF_CIRCULATION = &
codata_constant("quantum of circulation", &
real(3.6369475516d-4,dp), real(0.0000000011d-4,dp), &
"m^2 s^-1") !! quantum of circulation

type(codata_constant), parameter, public :: QUANTUM_OF_CIRCULATION_TIMES_2 = &
codata_constant("quantum of circulation times 2", &
real(7.2738951032d-4,dp), real(0.0000000022d-4,dp), &
"m^2 s^-1") !! quantum of circulation times 2

type(codata_constant), parameter, public :: REDUCED_COMPTON_WAVELENGTH = &
codata_constant("reduced Compton wavelength", &
real(3.8615926796d-13,dp), real(0.0000000012d-13,dp), &
"m") !! reduced Compton wavelength

type(codata_constant), parameter, public :: REDUCED_MUON_COMPTON_WAVELENGTH = &
codata_constant("reduced muon Compton wavelength", &
real(1.867594306d-15,dp), real(0.000000042d-15,dp), &
"m") !! reduced muon Compton wavelength

type(codata_constant), parameter, public :: REDUCED_NEUTRON_COMPTON_WAVELENGTH = &
codata_constant("reduced neutron Compton wavelength", &
real(2.1001941552d-16,dp), real(0.0000000012d-16,dp), &
"m") !! reduced neutron Compton wavelength

type(codata_constant), parameter, public :: REDUCED_PLANCK_CONSTANT = &
codata_constant("reduced Planck constant", &
real(1.054571817d-34,dp), real(0.0d0,dp), &
"J s") !! reduced Planck constant

type(codata_constant), parameter, public :: REDUCED_PLANCK_CONSTANT_IN_EV_S = &
codata_constant("reduced Planck constant in eV s", &
real(6.582119569d-16,dp), real(0.0d0,dp), &
"eV s") !! reduced Planck constant in eV s

type(codata_constant), parameter, public :: REDUCED_PLANCK_CONSTANT_TIMES_C_IN_MEV_FM = &
codata_constant("reduced Planck constant times c in MeV fm", &
real(197.3269804d0,dp), real(0.0d0,dp), &
"MeV fm") !! reduced Planck constant times c in MeV fm

type(codata_constant), parameter, public :: REDUCED_PROTON_COMPTON_WAVELENGTH = &
codata_constant("reduced proton Compton wavelength", &
real(2.10308910336d-16,dp), real(0.00000000064d-16,dp), &
"m") !! reduced proton Compton wavelength

type(codata_constant), parameter, public :: REDUCED_TAU_COMPTON_WAVELENGTH = &
codata_constant("reduced tau Compton wavelength", &
real(1.110538d-16,dp), real(0.000075d-16,dp), &
"m") !! reduced tau Compton wavelength

type(codata_constant), parameter, public :: RYDBERG_CONSTANT = &
codata_constant("Rydberg constant", &
real(10973731.568160d0,dp), real(0.000021d0,dp), &
"m^-1") !! Rydberg constant

type(codata_constant), parameter, public :: RYDBERG_CONSTANT_TIMES_C_IN_HZ = &
codata_constant("Rydberg constant times c in Hz", &
real(3.2898419602508d15,dp), real(0.0000000000064d15,dp), &
"Hz") !! Rydberg constant times c in Hz

type(codata_constant), parameter, public :: RYDBERG_CONSTANT_TIMES_HC_IN_EV = &
codata_constant("Rydberg constant times hc in eV", &
real(13.605693122994d0,dp), real(0.000000000026d0,dp), &
"eV") !! Rydberg constant times hc in eV

type(codata_constant), parameter, public :: RYDBERG_CONSTANT_TIMES_HC_IN_J = &
codata_constant("Rydberg constant times hc in J", &
real(2.1798723611035d-18,dp), real(0.0000000000042d-18,dp), &
"J") !! Rydberg constant times hc in J

type(codata_constant), parameter, public :: SACKUR_TETRODE_CONSTANT_1_K_100_KPA = &
codata_constant("Sackur-Tetrode constant (1 K, 100 kPa)", &
real(-1.15170753706d0,dp), real(0.00000000045d0,dp), &
"") !! Sackur-Tetrode constant (1 K, 100 kPa)

type(codata_constant), parameter, public :: SACKUR_TETRODE_CONSTANT_1_K_101325_KPA = &
codata_constant("Sackur-Tetrode constant (1 K, 101.325 kPa)", &
real(-1.16487052358d0,dp), real(0.00000000045d0,dp), &
"") !! Sackur-Tetrode constant (1 K, 101.325 kPa)

type(codata_constant), parameter, public :: SECOND_RADIATION_CONSTANT = &
codata_constant("second radiation constant", &
real(1.438776877d-2,dp), real(0.0d0,dp), &
"m K") !! second radiation constant

type(codata_constant), parameter, public :: SHIELDED_HELION_GYROMAG_RATIO = &
codata_constant("shielded helion gyromag. ratio", &
real(2.037894569d8,dp), real(0.000000024d8,dp), &
"s^-1 T^-1") !! shielded helion gyromag. ratio

type(codata_constant), parameter, public :: SHIELDED_HELION_GYROMAG_RATIO_IN_MHZ_T = &
codata_constant("shielded helion gyromag. ratio in MHz/T", &
real(32.43409942d0,dp), real(0.00000038d0,dp), &
"MHz T^-1") !! shielded helion gyromag. ratio in MHz/T

type(codata_constant), parameter, public :: SHIELDED_HELION_MAG_MOM = &
codata_constant("shielded helion mag. mom.", &
real(-1.074553090d-26,dp), real(0.000000013d-26,dp), &
"J T^-1") !! shielded helion mag. mom.

type(codata_constant), parameter, public :: SHIELDED_HELION_MAG_MOM_TO_BOHR_MAGNETON_RATIO = &
codata_constant("shielded helion mag. mom. to Bohr magneton ratio", &
real(-1.158671471d-3,dp), real(0.000000014d-3,dp), &
"") !! shielded helion mag. mom. to Bohr magneton ratio

type(codata_constant), parameter, public :: SHIELDED_HELION_MAG_MOM_TO_NUCLEAR_MAGNETON_RATIO = &
codata_constant("shielded helion mag. mom. to nuclear magneton ratio", &
real(-2.127497719d0,dp), real(0.000000025d0,dp), &
"") !! shielded helion mag. mom. to nuclear magneton ratio

type(codata_constant), parameter, public :: SHIELDED_HELION_TO_PROTON_MAG_MOM_RATIO = &
codata_constant("shielded helion to proton mag. mom. ratio", &
real(-0.7617665618d0,dp), real(0.0000000089d0,dp), &
"") !! shielded helion to proton mag. mom. ratio

type(codata_constant), parameter, public :: SHIELDED_HELION_TO_SHIELDED_PROTON_MAG_MOM_RATIO = &
codata_constant("shielded helion to shielded proton mag. mom. ratio", &
real(-0.7617861313d0,dp), real(0.0000000033d0,dp), &
"") !! shielded helion to shielded proton mag. mom. ratio

type(codata_constant), parameter, public :: SHIELDED_PROTON_GYROMAG_RATIO = &
codata_constant("shielded proton gyromag. ratio", &
real(2.675153151d8,dp), real(0.000000029d8,dp), &
"s^-1 T^-1") !! shielded proton gyromag. ratio

type(codata_constant), parameter, public :: SHIELDED_PROTON_GYROMAG_RATIO_IN_MHZ_T = &
codata_constant("shielded proton gyromag. ratio in MHz/T", &
real(42.57638474d0,dp), real(0.00000046d0,dp), &
"MHz T^-1") !! shielded proton gyromag. ratio in MHz/T

type(codata_constant), parameter, public :: SHIELDED_PROTON_MAG_MOM = &
codata_constant("shielded proton mag. mom.", &
real(1.410570560d-26,dp), real(0.000000015d-26,dp), &
"J T^-1") !! shielded proton mag. mom.

type(codata_constant), parameter, public :: SHIELDED_PROTON_MAG_MOM_TO_BOHR_MAGNETON_RATIO = &
codata_constant("shielded proton mag. mom. to Bohr magneton ratio", &
real(1.520993128d-3,dp), real(0.000000017d-3,dp), &
"") !! shielded proton mag. mom. to Bohr magneton ratio

type(codata_constant), parameter, public :: SHIELDED_PROTON_MAG_MOM_TO_NUCLEAR_MAGNETON_RATIO = &
codata_constant("shielded proton mag. mom. to nuclear magneton ratio", &
real(2.792775599d0,dp), real(0.000000030d0,dp), &
"") !! shielded proton mag. mom. to nuclear magneton ratio

type(codata_constant), parameter, public :: SHIELDING_DIFFERENCE_OF_D_AND_P_IN_HD = &
codata_constant("shielding difference of d and p in HD", &
real(2.0200d-8,dp), real(0.0020d-8,dp), &
"") !! shielding difference of d and p in HD

type(codata_constant), parameter, public :: SHIELDING_DIFFERENCE_OF_T_AND_P_IN_HT = &
codata_constant("shielding difference of t and p in HT", &
real(2.4140d-8,dp), real(0.0020d-8,dp), &
"") !! shielding difference of t and p in HT

type(codata_constant), parameter, public :: SPEED_OF_LIGHT_IN_VACUUM = &
codata_constant("speed of light in vacuum", &
real(299792458d0,dp), real(0.0d0,dp), &
"m s^-1") !! speed of light in vacuum

type(codata_constant), parameter, public :: STANDARD_ACCELERATION_OF_GRAVITY = &
codata_constant("standard acceleration of gravity", &
real(9.80665d0,dp), real(0.0d0,dp), &
"m s^-2") !! standard acceleration of gravity

type(codata_constant), parameter, public :: STANDARD_ATMOSPHERE = &
codata_constant("standard atmosphere", &
real(101325d0,dp), real(0.0d0,dp), &
"Pa") !! standard atmosphere

type(codata_constant), parameter, public :: STANDARD_STATE_PRESSURE = &
codata_constant("standard-state pressure", &
real(100000d0,dp), real(0.0d0,dp), &
"Pa") !! standard-state pressure

type(codata_constant), parameter, public :: STEFAN_BOLTZMANN_CONSTANT = &
codata_constant("Stefan-Boltzmann constant", &
real(5.670374419d-8,dp), real(0.0d0,dp), &
"W m^-2 K^-4") !! Stefan-Boltzmann constant

type(codata_constant), parameter, public :: TAU_COMPTON_WAVELENGTH = &
codata_constant("tau Compton wavelength", &
real(6.97771d-16,dp), real(0.00047d-16,dp), &
"m") !! tau Compton wavelength

type(codata_constant), parameter, public :: TAU_ELECTRON_MASS_RATIO = &
codata_constant("tau-electron mass ratio", &
real(3477.23d0,dp), real(0.23d0,dp), &
"") !! tau-electron mass ratio

type(codata_constant), parameter, public :: TAU_ENERGY_EQUIVALENT = &
codata_constant("tau energy equivalent", &
real(1776.86d0,dp), real(0.12d0,dp), &
"MeV") !! tau energy equivalent

type(codata_constant), parameter, public :: TAU_MASS = &
codata_constant("tau mass", &
real(3.16754d-27,dp), real(0.00021d-27,dp), &
"kg") !! tau mass

type(codata_constant), parameter, public :: TAU_MASS_ENERGY_EQUIVALENT = &
codata_constant("tau mass energy equivalent", &
real(2.84684d-10,dp), real(0.00019d-10,dp), &
"J") !! tau mass energy equivalent

type(codata_constant), parameter, public :: TAU_MASS_IN_U = &
codata_constant("tau mass in u", &
real(1.90754d0,dp), real(0.00013d0,dp), &
"u") !! tau mass in u

type(codata_constant), parameter, public :: TAU_MOLAR_MASS = &
codata_constant("tau molar mass", &
real(1.90754d-3,dp), real(0.00013d-3,dp), &
"kg mol^-1") !! tau molar mass

type(codata_constant), parameter, public :: TAU_MUON_MASS_RATIO = &
codata_constant("tau-muon mass ratio", &
real(16.8170d0,dp), real(0.0011d0,dp), &
"") !! tau-muon mass ratio

type(codata_constant), parameter, public :: TAU_NEUTRON_MASS_RATIO = &
codata_constant("tau-neutron mass ratio", &
real(1.89115d0,dp), real(0.00013d0,dp), &
"") !! tau-neutron mass ratio

type(codata_constant), parameter, public :: TAU_PROTON_MASS_RATIO = &
codata_constant("tau-proton mass ratio", &
real(1.89376d0,dp), real(0.00013d0,dp), &
"") !! tau-proton mass ratio

type(codata_constant), parameter, public :: THOMSON_CROSS_SECTION = &
codata_constant("Thomson cross section", &
real(6.6524587321d-29,dp), real(0.0000000060d-29,dp), &
"m^2") !! Thomson cross section

type(codata_constant), parameter, public :: TRITON_ELECTRON_MASS_RATIO = &
codata_constant("triton-electron mass ratio", &
real(5496.92153573d0,dp), real(0.00000027d0,dp), &
"") !! triton-electron mass ratio

type(codata_constant), parameter, public :: TRITON_G_FACTOR = &
codata_constant("triton g factor", &
real(5.957924931d0,dp), real(0.000000012d0,dp), &
"") !! triton g factor

type(codata_constant), parameter, public :: TRITON_MAG_MOM = &
codata_constant("triton mag. mom.", &
real(1.5046095202d-26,dp), real(0.0000000030d-26,dp), &
"J T^-1") !! triton mag. mom.

type(codata_constant), parameter, public :: TRITON_MAG_MOM_TO_BOHR_MAGNETON_RATIO = &
codata_constant("triton mag. mom. to Bohr magneton ratio", &
real(1.6223936651d-3,dp), real(0.0000000032d-3,dp), &
"") !! triton mag. mom. to Bohr magneton ratio

type(codata_constant), parameter, public :: TRITON_MAG_MOM_TO_NUCLEAR_MAGNETON_RATIO = &
codata_constant("triton mag. mom. to nuclear magneton ratio", &
real(2.9789624656d0,dp), real(0.0000000059d0,dp), &
"") !! triton mag. mom. to nuclear magneton ratio

type(codata_constant), parameter, public :: TRITON_MASS = &
codata_constant("triton mass", &
real(5.0073567446d-27,dp), real(0.0000000015d-27,dp), &
"kg") !! triton mass

type(codata_constant), parameter, public :: TRITON_MASS_ENERGY_EQUIVALENT = &
codata_constant("triton mass energy equivalent", &
real(4.5003878060d-10,dp), real(0.0000000014d-10,dp), &
"J") !! triton mass energy equivalent

type(codata_constant), parameter, public :: TRITON_MASS_ENERGY_EQUIVALENT_IN_MEV = &
codata_constant("triton mass energy equivalent in MeV", &
real(2808.92113298d0,dp), real(0.00000085d0,dp), &
"MeV") !! triton mass energy equivalent in MeV

type(codata_constant), parameter, public :: TRITON_MASS_IN_U = &
codata_constant("triton mass in u", &
real(3.01550071621d0,dp), real(0.00000000012d0,dp), &
"u") !! triton mass in u

type(codata_constant), parameter, public :: TRITON_MOLAR_MASS = &
codata_constant("triton molar mass", &
real(3.01550071517d-3,dp), real(0.00000000092d-3,dp), &
"kg mol^-1") !! triton molar mass

type(codata_constant), parameter, public :: TRITON_PROTON_MASS_RATIO = &
codata_constant("triton-proton mass ratio", &
real(2.99371703414d0,dp), real(0.00000000015d0,dp), &
"") !! triton-proton mass ratio

type(codata_constant), parameter, public :: TRITON_RELATIVE_ATOMIC_MASS = &
codata_constant("triton relative atomic mass", &
real(3.01550071621d0,dp), real(0.00000000012d0,dp), &
"") !! triton relative atomic mass

type(codata_constant), parameter, public :: TRITON_TO_PROTON_MAG_MOM_RATIO = &
codata_constant("triton to proton mag. mom. ratio", &
real(1.0666399191d0,dp), real(0.0000000021d0,dp), &
"") !! triton to proton mag. mom. ratio

type(codata_constant), parameter, public :: UNIFIED_ATOMIC_MASS_UNIT = &
codata_constant("unified atomic mass unit", &
real(1.66053906660d-27,dp), real(0.00000000050d-27,dp), &
"kg") !! unified atomic mass unit

type(codata_constant), parameter, public :: VACUUM_ELECTRIC_PERMITTIVITY = &
codata_constant("vacuum electric permittivity", &
real(8.8541878128d-12,dp), real(0.0000000013d-12,dp), &
"F m^-1") !! vacuum electric permittivity

type(codata_constant), parameter, public :: VACUUM_MAG_PERMEABILITY = &
codata_constant("vacuum mag. permeability", &
real(1.25663706212d-6,dp), real(0.00000000019d-6,dp), &
"N A^-2") !! vacuum mag. permeability

type(codata_constant), parameter, public :: VON_KLITZING_CONSTANT = &
codata_constant("von Klitzing constant", &
real(25812.80745d0,dp), real(0.0d0,dp), &
"ohm") !! von Klitzing constant

type(codata_constant), parameter, public :: WEAK_MIXING_ANGLE = &
codata_constant("weak mixing angle", &
real(0.22290d0,dp), real(0.00030d0,dp), &
"") !! weak mixing angle

type(codata_constant), parameter, public :: WIEN_FREQUENCY_DISPLACEMENT_LAW_CONSTANT = &
codata_constant("Wien frequency displacement law constant", &
real(5.878925757d10,dp), real(0.0d0,dp), &
"Hz K^-1") !! Wien frequency displacement law constant

type(codata_constant), parameter, public :: WIEN_WAVELENGTH_DISPLACEMENT_LAW_CONSTANT = &
codata_constant("Wien wavelength displacement law constant", &
real(2.897771955d-3,dp), real(0.0d0,dp), &
"m K") !! Wien wavelength displacement law constant

type(codata_constant), parameter, public :: W_TO_Z_MASS_RATIO = &
codata_constant("W to Z mass ratio", &
real(0.88153d0,dp), real(0.00017d0,dp), &
"") !! W to Z mass ratio

end module stdlib_codata