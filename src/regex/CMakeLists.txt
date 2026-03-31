# CMakeLists.txt for stdlib_regex

set(srcs
  stdlib_regex.f90
)

add_library(stdlib_regex ${srcs})
target_link_libraries(stdlib_regex PRIVATE stdlib_kinds)
