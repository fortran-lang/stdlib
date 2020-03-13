# ================================
# Set gfortran compile flags
# ================================
message(STATUS "Getting gfortran flags")

# Set flags for all build types
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008ts -cpp -ffree-line-length-none -fall-intrinsics")

# ================================
# Set GFORTRAN flags for a SHARED library
# ================================
if(BUILD_SHARED_LIBS)
  # Add any shared library related stuff here
  set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -shared -fpic")

  if(LINUX OR APPLE)
    # Taken from: https://cmake.org/Wiki/CMake_RPATH_handling#Mac_OS_X_and_the_RPATH
    # use, i.e. don't skip the full RPATH for the build tree
    set(CMAKE_SKIP_BUILD_RPATH FALSE)

    # when building, don't use the install RPATH already
    # (but later on when installing)
    set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

    set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

    # add the automatically determined parts of the RPATH
    # which point to directories outside the build tree to the install RPATH
    set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

    # the RPATH to be used when installing, but only if it's not a system directory
    list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
    if("${isSystemDir}" STREQUAL "-1")
      set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
    endif("${isSystemDir}" STREQUAL "-1")
  endif()

# ================================
# Set GFORTRAN flags for a STATIC library
# ================================
else()
  # Static build options
  if(APPLE)
    # gcc on OS X defaults to the dynamic quadmath library instead of the static
    # NOTE: LIBRARY_PATH environment variable must be properly defined for the
    #   current compiler or find_library(quadmath) will return the static
    #   system version of libquadmath.a
    set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
    find_library(LIB_QUADMATH quadmath)

    if ("${LIB_QUADMATH}" STREQUAL "LIB_QUADMATH-NOTFOUND")
      string(CONCAT quadmath_error "\
Could not find the quadmath library. \n \
You might need to add to or create the environment variable CMAKE_LIBRARY_PATH. \n \
i.e. export CMAKE_LIBRARY_PATH=$CMAKE_LIBRARY_PATH:/path/to/gfortran/folder/lib \n \
You can find which folder contains libquadmath.a/so by typing 'gcc -### -Xlinker -v 2>&1 | grep LIBRARY' or perhaps 'readlink gcc' \n \
Search through those folders and libquadmath should be in a 'lib' folder.  That path is what should be added to CMAKE_LIBRARY_PATH' \n \
Make sure that gcc is the GNU compiler and not OSX's clang compiler by typing 'gcc --version' \n")
      message(FATAL_ERROR ${quadmath_error})
    else()
    endif()

    message(STATUS "quadmath library path: ${LIB_QUADMATH}")


    # TODO: The addition of '-lgcc_s.1' is a messy fix for libgcc_s.1.dylib
    #       not being properly included as an indirect dependency of
    #       libquadmath.a. This is a brittle hack that needs to be fixed.
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libgfortran -static-libgcc -lgfortran -lgcc -lgcc_s.1 -lSystem -nodefaultlibs ${LIB_QUADMATH}")

    # Apple's ar and ranlib commands toss out 'no symbols' warnings
    # The following two lines quiets those warnings
    set(CMAKE_Fortran_ARCHIVE_CREATE "<CMAKE_AR> Scr <TARGET> <LINK_FLAGS> <OBJECTS>")
    set(CMAKE_Fortran_ARCHIVE_FINISH "<CMAKE_RANLIB> -no_warning_for_no_symbols -c <TARGET>")
  elseif(WIN32)
    set(CMAKE_FIND_LIBRARY_SUFFIXES ".lib")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libgfortran -static-libgcc")
  elseif(${LINUX})
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static -static-libgfortran -static-libgcc -lgfortran -lgcc")
  endif()
endif()

set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -funroll-all-loops -finline-functions")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -fbacktrace -fbounds-check -Waliasing -Wampersand -Wconversion -Wsurprising -Wc-binding-type -Wintrinsics-std -Wtabs -Wintrinsic-shadow -Wline-truncation -Wtarget-lifetime -Wreal-q-constant")

if(APPLE)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-underscoring")
endif()