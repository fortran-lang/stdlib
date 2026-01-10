
!Default: compile the bitset module
#if !defined STDLIB_BITSET
#define STDLIB_BITSET 1
#endif

!Default: compile the hashmaps module
#ifdef STDLIB_NO_HASHMAPS
#define STDLIB_HASHMAPS 0
#else
#define STDLIB_HASHMAPS 1
#endif

!Default: compile the stats module
#if !defined STDLIB_STATS
#define STDLIB_STATS 1
#endif
