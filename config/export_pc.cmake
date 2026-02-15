# Export a pkg-config file

# Inspect linked libraries
function(resolve_pc_libs out_var root_target)
    set(_result "")
    set(_visited "")

    # List of imported targets to skip (handled separately for pkg-config)
    set(_skip_targets "BLAS::BLAS" "LAPACK::LAPACK")

    function(_resolve target)
        # Prevent infinite recursion
        if(target IN_LIST _visited)
            return()
        endif()
        list(APPEND _visited "${target}")
        set(_visited "${_visited}" PARENT_SCOPE)

        # Skip BLAS/LAPACK imported targets — they will be handled
        # separately via Requires.private / Libs.private
        if(target IN_LIST _skip_targets)
            return()
        endif()

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

    # Remove the duplicates by keeping the first occurrence
    list(REMOVE_DUPLICATES _result)

    # Reverse the order
    list(REVERSE _result)

    set(${out_var} "${_result}" PARENT_SCOPE)
endfunction()

# Convert absolute library paths to -l flags
# e.g. /usr/lib/libopenblas.so       -> -lopenblas
#      /usr/lib/libopenblas.so.0.3   -> -lopenblas
#      /usr/lib/libopenblas.dll.a    -> -lopenblas
function(libs_to_linker_flags out_var libs)
    set(_flags "")
    foreach(lib IN LISTS libs)
        if(IS_ABSOLUTE "${lib}")
            # Extract the full filename, then strip everything from the first dot
            # onwards. Using NAME_WE would only strip the last extension, which
            # breaks for versioned libraries (e.g. libopenblas.so.0.3 -> libopenblas.so.0).
            get_filename_component(_name "${lib}" NAME)
            string(REGEX REPLACE "\\..*$" "" _name "${_name}")
            # Strip leading "lib" prefix if present
            string(REGEX REPLACE "^lib" "" _name "${_name}")
            list(APPEND _flags "-l${_name}")
        else()
            # Already a flag like -lopenblas or -lm
            list(APPEND _flags "${lib}")
        endif()
    endforeach()
    list(REMOVE_DUPLICATES _flags)
    set(${out_var} "${_flags}" PARENT_SCOPE)
endfunction()

resolve_pc_libs(PC_LIBS ${PROJECT_NAME})

string(REPLACE ";" " " PC_CONTENT "${PC_LIBS}")

# --- Handle BLAS/LAPACK for pkg-config ---
# Use Requires.private when a vendor .pc file exists,
# otherwise fall back to Libs.private with -l flags.
set(PC_REQUIRES_PRIVATE "")
set(PC_LIBS_PRIVATE "")

if(BLAS_FOUND OR LAPACK_FOUND)
    find_package(PkgConfig QUIET)

    # Known pkg-config names for common BLAS/LAPACK vendors.
    # We try the most specific names first, then fall back to generic ones.
    set(_blas_pc_names
        openblas           # OpenBLAS
        blas-openblas      # Debian/Ubuntu OpenBLAS
        mkl-dynamic-lp64-seq   # Intel MKL (dynamic, LP64, sequential)
        mkl-dynamic-ilp64-seq  # Intel MKL (dynamic, ILP64, sequential)
        mkl-dynamic-lp64-iomp  # Intel MKL (dynamic, LP64, OpenMP)
        mkl-dynamic-ilp64-iomp # Intel MKL (dynamic, ILP64, OpenMP)
        mkl-static-lp64-seq    # Intel MKL (static, LP64, sequential)
        mkl-static-ilp64-seq   # Intel MKL (static, ILP64, sequential)
        blas                   # Generic BLAS
        blas-netlib            # Netlib BLAS
    )
    set(_lapack_pc_names
        lapack             # Generic LAPACK
        lapack-netlib      # Netlib LAPACK
    )

    # BLAS packages that are known to also provide LAPACK
    set(_blas_includes_lapack_list
        openblas
        blas-openblas
        mkl-dynamic-lp64-seq
        mkl-dynamic-ilp64-seq
        mkl-dynamic-lp64-iomp
        mkl-dynamic-ilp64-iomp
        mkl-static-lp64-seq
        mkl-static-ilp64-seq
    )

    set(_blas_pc_found FALSE)
    set(_blas_includes_lapack FALSE)
    set(_lapack_pc_found FALSE)

    if(PKG_CONFIG_FOUND)
        # --- Detect BLAS pkg-config package ---
        if(BLAS_FOUND)
            foreach(_pkg IN LISTS _blas_pc_names)
                pkg_check_modules(_BLAS_PC_${_pkg} QUIET "${_pkg}")
                if(_BLAS_PC_${_pkg}_FOUND)
                    list(APPEND PC_REQUIRES_PRIVATE "${_pkg}")
                    set(_blas_pc_found TRUE)
                    if(_pkg IN_LIST _blas_includes_lapack_list)
                        set(_blas_includes_lapack TRUE)
                    endif()
                    message(STATUS "pkg-config: using '${_pkg}' for BLAS")
                    break()
                endif()
            endforeach()
        endif()

        # --- Detect LAPACK pkg-config package ---
        # Skip separate LAPACK detection if the BLAS package already includes LAPACK
        # (e.g. OpenBLAS, MKL). Standalone BLAS packages like netlib still need this.
        if(LAPACK_FOUND AND NOT _blas_includes_lapack)
            foreach(_pkg IN LISTS _lapack_pc_names)
                pkg_check_modules(_LAPACK_PC_${_pkg} QUIET "${_pkg}")
                if(_LAPACK_PC_${_pkg}_FOUND)
                    list(APPEND PC_REQUIRES_PRIVATE "${_pkg}")
                    set(_lapack_pc_found TRUE)
                    message(STATUS "pkg-config: using '${_pkg}' for LAPACK")
                    break()
                endif()
            endforeach()
        endif()
    endif()

    # --- Fallback: convert absolute paths to -l flags ---
    if(BLAS_FOUND AND NOT _blas_pc_found)
        libs_to_linker_flags(_blas_flags "${BLAS_LIBRARIES}")
        list(APPEND PC_LIBS_PRIVATE ${_blas_flags})
        message(STATUS "pkg-config: no .pc file found for BLAS, using linker flags: ${_blas_flags}")
    endif()

    if(LAPACK_FOUND AND NOT _lapack_pc_found AND NOT _blas_includes_lapack)
        libs_to_linker_flags(_lapack_flags "${LAPACK_LIBRARIES}")
        list(APPEND PC_LIBS_PRIVATE ${_lapack_flags})
        message(STATUS "pkg-config: no .pc file found for LAPACK, using linker flags: ${_lapack_flags}")
    endif()
endif()

# Flatten lists to space-separated strings
string(REPLACE ";" " " PC_REQUIRES_PRIVATE "${PC_REQUIRES_PRIVATE}")
string(REPLACE ";" " " PC_LIBS_PRIVATE "${PC_LIBS_PRIVATE}")

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
