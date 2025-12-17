
!Default: compile the bitset module
#ifdef STDLIB_NO_BITSET
#define STDLIB_BITSET 0
#else
#define STDLIB_BITSET 1
#endif

!Default: compile the stats module
#ifdef STDLIB_NO_STATS
#define STDLIB_STATS 0
#else
#define STDLIB_STATS 1
#endif
