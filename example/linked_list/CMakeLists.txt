#include_directories(${CMAKE_CURRENT_LIST_DIR})

macro(ADD_EXAMPLE_INCLUDE name)
    add_executable(example_${name} example_${name}.f90)
    target_include_drectories(example_${name} ${CMAKE_CURRENT_SOURCE_DIR})
    target_link_libraries(example_${name} "${PROJECT_NAME}")
    add_test(NAME ${name}
             COMMAND $<TARGET_FILE:example_${name}> ${CMAKE_CURRENT_BINARY_DIR}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro(ADD_EXAMPLE_INCLUDE)

ADD_EXAMPLE_INCLUDE(linked_absorb)
ADD_EXAMPLE_INCLUDE(linked_clear)
ADD_EXAMPLE_INCLUDE(linked_concat)
ADD_EXAMPLE_INCLUDE(linked_get)
ADD_EXAMPLE_INCLUDE(linked_insert)
ADD_EXAMPLE_INCLUDE(linked_pop)
ADD_EXAMPLE_INCLUDE(linked_push)
ADD_EXAMPLE_INCLUDE(linked_remove)
ADD_EXAMPLE_INCLUDE(linked_replace)
ADD_EXAMPLE_INCLUDE(linked_reverse)
ADD_EXAMPLE_INCLUDE(linked_size)
ADD_EXAMPLE_INCLUDE(linked_slice)
ADD_EXAMPLE_INCLUDE(linked_splice)
