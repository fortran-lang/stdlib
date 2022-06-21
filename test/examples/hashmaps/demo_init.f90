program demo_init
use stdlib_hashmaps, only: chaining_map_type
use stdlib_hashmap_wrappers, only: fnv_1_hasher
type(fnv_1a_type)       :: fnv_1
type(chaining_map_type) :: map
call map % init( fnv_1a, slots_bits=10 )
end program demo_init
