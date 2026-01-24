! Helper macro: define a feature flag with default value 1
! This will enable the compulation of the correspond module
#define STDLIB_DEFAULT(name) \
#if !defined name          \
#define name 1              \
#endif

STDLIB_DEFAULT(STDLIB_ANSI)
STDLIB_DEFAULT(STDLIB_BITSET)
STDLIB_DEFAULT(STDLIB_HASHMAPS)
STDLIB_DEFAULT(STDLIB_STATS)
