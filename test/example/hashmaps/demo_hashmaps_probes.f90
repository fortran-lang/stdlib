program demo_probes
use stdlib_hashmaps, only: chaining_hashmap_type, int_index
use stdlib_hashmap_wrappers, only: fnv_1_hasher
implicit none
type(chaining_hashmap_type) :: map
real :: nprobes
call map % init( fnv_1_hasher )
nprobes = map % map_probes()
print *, "Initial probes =  ", nprobes
end program demo_probes
