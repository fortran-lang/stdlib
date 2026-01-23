# Export a pkg-config file

# Inspect linked libraries
function(resolve_pc_libs out_var root_target)
    set(_result "")
    set(_visited "")

    function(_resolve target)
        # Prevent infinite recursion
        if(target IN_LIST _visited)
            return()
        endif()
        list(APPEND _visited "${target}")
        set(_visited "${_visited}" PARENT_SCOPE)

        if(TARGET "${target}")
            # Recurse into PUBLIC/INTERFACE deps first
            get_target_property(deps "${target}" INTERFACE_LINK_LIBRARIES)
            if(deps)
                foreach(dep IN LISTS deps)
                    _resolve("${dep}")
                endforeach()
            endif()

            # Now append the target itself (if it produces a library)
            get_target_property(type "${target}" TYPE)
            if(type MATCHES "STATIC_LIBRARY|SHARED_LIBRARY")
                get_target_property(name "${target}" OUTPUT_NAME)
                if(NOT name)
                    set(name "${target}")
                endif()
                list(APPEND _result "-l${name}")
            endif()
        else()
            # Plain linker flag or library
            list(APPEND _result "${target}")
        endif()

        set(_result "${_result}" PARENT_SCOPE)
    endfunction()

    _resolve("${root_target}")

    # Reverse the order
    list(REVERSE _result)
    #list(REMOVE_DUPLICATES _result)

    set(${out_var} "${_result}" PARENT_SCOPE)
endfunction()

resolve_pc_libs(PC_LIBS ${PROJECT_NAME})

string(REPLACE ";" " " PC_CONTENT "${PC_LIBS}")

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/config/template.pc"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc"
  @ONLY
)
install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig"
)

