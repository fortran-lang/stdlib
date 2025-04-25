# SPDX-Identifier: MIT

if(NOT DEFINED CMAKE_INSTALL_MODULEDIR)
  set(
    CMAKE_INSTALL_MODULEDIR
    "${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${CMAKE_Fortran_COMPILER_ID}-${CMAKE_Fortran_COMPILER_VERSION}"
    CACHE
    STRING
    "Directory in prefix to install generated module files"
  )
endif()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" PARENT_SCOPE)

# Check for available features
# Note: users can overwrite the automatic check by setting the value at configure time
include(CheckFortranSourceRuns)
if (NOT DEFINED WITH_CBOOL)
  check_fortran_source_runs(
    "use, intrinsic :: iso_c_binding, only: c_bool; integer, parameter :: lk = kind(.true.)
     if (c_bool == lk) stop 1
     end"
    WITH_CBOOL
  )
  set(WITH_CBOOL ${WITH_CBOOL} PARENT_SCOPE)
endif()
if (NOT DEFINED WITH_QP)
  check_fortran_source_runs(
    "if (selected_real_kind(33) == -1) stop 1; end"
    WITH_QP
  )
  set(WITH_QP ${WITH_QP} PARENT_SCOPE)
endif()
if (NOT DEFINED WITH_XDP)
  check_fortran_source_runs(
    "if (any(selected_real_kind(18) == [-1, selected_real_kind(33)])) stop 1; end"
    WITH_XDP
  )
  set(WITH_XDP ${WITH_XDP} PARENT_SCOPE)
endif()
# Check if WITH_ILP64 is defined; if not, set it to FALSE
if (NOT DEFINED WITH_ILP64)
  set(WITH_ILP64 FALSE)
  set(WITH_ILP64 ${WITH_ILP64} PARENT_SCOPE)
endif()

# Export a pkg-config file
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/template.pc"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc"
  @ONLY
)
install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig"
)

# Export CMake package file
include(CMakePackageConfigHelpers)
configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/template.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)
if(BUILD_SHARED_LIBS OR PROJECT_VERSION_MAJOR EQUAL 0)
  # Due to the uncertain ABI compatibility of Fortran shared libraries
  # limit compatibility for dynamic linking to same minor version.
  set(COMPATIBILITY SameMinorVersion)
else()
  # Require API compatibility via semantic versioning for static linking.
  set(COMPATIBILITY SameMajorVersion)
endif()
write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY ${COMPATIBILITY}
)
install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)
