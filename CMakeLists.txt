cmake_minimum_required(VERSION 3.17.5)

# define build environments
set( CMAKE_INSTALL_PREFIX "$ENV{HOME}/.local/graphstruc"
  CACHE STRING "Select where to install the library." )
execute_process(COMMAND pwd OUTPUT_VARIABLE CURRENT_WORKING_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
message("Current working directory: ${CURRENT_WORKING_DIR}")
set( CMAKE_BUILD_PREFIX ${CURRENT_WORKING_DIR}
  CACHE STRING "Select where to build the library." )
set(MODULE_DIR ${CMAKE_BUILD_PREFIX}/mod)

# set the project name
project(graphstruc NONE)

# set the library name
set( LIB_NAME ${PROJECT_NAME} )
set( PROJECT_DESCRIPTION
  "Fortran neural network" )
set( PROJECT_URL "https://github.com/nedtaylor/graphstruc" )
set( CMAKE_CONFIGURATION_TYPES "Release" "Parallel" "Serial" "Dev" "Debug"
  CACHE STRING "List of configurations types." )
set( CMAKE_BUILD_TYPE "Release"
  CACHE STRING "Select which configuration to build." )

# change name based on parallel
if (CMAKE_BUILD_TYPE MATCHES "Parallel*")
   project(graphstruc_omp NONE) # change project name to parallel
   message(FATAL_ERROR "Configuration stopped because Parallel is not yet set up")
endif()

# set compiler
set(CMAKE_Fortran_COMPILER gfortran
  CACHE STRING "Select fortran compiler." )  # Change this to your desired compiler
set(CMAKE_Fortran_STANDARD 2018)

# set language
enable_language(Fortran)

# get the user's home directory
set(HOME_DIR $ENV{HOME})

# set options for building tests and examples
option(BUILD_TESTS "Build the unit tests" ON)
option(BUILD_EXAMPLES "Build the examples" ON)

# set coverage compiler flags
if (CMAKE_BUILD_TYPE MATCHES "Debug*" OR CMAKE_BUILD_TYPE MATCHES "Dev*")
  list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
  set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
      include(CodeCoverage)
      setup_target_for_coverage_gcovr_html(
        NAME coverage
        EXECUTABLE ctest
        EXCLUDE "${PROJECT_SOURCE_DIR}/test/*")
  endif()
endif()

# enable testing
enable_testing()

# set the output directories
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# Define the sources
set(SRC_DIR src)
set(FORTRAN_SRC_DIR ${SRC_DIR})


set(SRC_FILES
    graphstruc_kinds.f90
    graphstruc.f90
    graphstruc_sub.f90
)
foreach(src ${SRC_FILES})
    list(APPEND PREPENDED_SRC_FILES ${FORTRAN_SRC_DIR}/${src})
endforeach()


# initialise flags
set(CPPFLAGS "")
set(CFLAGS "")
set(MODULEFLAGS "")
set(MPFLAGS "")
set(WARNFLAGS "")
set(DEVFLAGS "")
set(DEBUGFLAGS "")
set(MEMFLAGS "")
set(OPTIMFLAGS "")
set(FASTFLAGS "")

# set flags based on compiler
if (CMAKE_Fortran_COMPILER MATCHES ".*gfortran.*" OR CMAKE_Fortran_COMPILER MATCHES ".*gcc.*")
    message(STATUS "Using gfortran compiler")
    set(PPFLAGS -cpp)
    set(MPFLAGS -fopenmp)
    set(WARNFLAGS -Wall)
    set(DEVFLAGS -g -fbacktrace -fcheck=all -fbounds-check -Og)
    set(DEBUGFLAGS -fbounds-check)
    set(MEMFLAGS -mcmodel=large)
    set(OPTIMFLAGS -O3 -march=native)
    set(FASTFLAGS -Ofast -march=native)
    set(PYTHONFLAGS -c -O3 -fPIC)
elseif (CMAKE_Fortran_COMPILER MATCHES ".*nag.*")
    message(STATUS "Using nag compiler")
    set(PPFLAGS -f2018 -fpp)
    set(MPFLAGS -openmp)
    set(WARNFLAGS -Wall)
    set(DEVFLAGS -g -mtrace -C=all -colour -O0)
    set(DEBUGFLAGS -C=array)
    set(MEMFLAGS -mcmodel=large)
    set(OPTIMFLAGS -O3)
    set(FASTFLAGS -Ofast)
elseif (CMAKE_Fortran_COMPILER MATCHES ".*ifort.*" OR CMAKE_Fortran_COMPILER MATCHES ".*ifx.*")
    message(STATUS "Using intel compiler")
    set(PPFLAGS -fpp)
    set(MPFLAGS -qopenmp)
    set(WARNFLAGS -warn all)
    set(DEVFLAGS -check all -warn)
    set(DEBUGFLAGS -check all -fpe0 -warn -tracekback -debug extended)
    set(MEMFLAGS -mcmodel=large)
    set(OPTIMFLAGS -O3)
    set(FASTFLAGS -Ofast)
else()
    # Code for other Fortran compilers
    message(STATUS "Using a different Fortran compiler")
endif()





set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${PPFLAGS}")


# create the library
add_library(${PROJECT_NAME} STATIC ${PREPENDED_SRC_FILES})
set_target_properties(${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIR})
target_link_libraries(${PROJECT_NAME} PUBLIC)

# replace ".f90" with ".mod"
string(REGEX REPLACE "\\.[^.]*$" ".mod" MODULE_FILES "${SRC_FILES}")

# installation
install(FILES ${MODULE_DIR}/${MODULE_FILES} DESTINATION include)
install(TARGETS ${PROJECT_NAME} DESTINATION lib)

# set compile options based on different build configurations
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Release>:${OPTIMFLAGS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Parallel>:${OPTIMFLAGS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Parallel>:${MPFLAGS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Dev>:${DEVFLAGS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Debug>:${DEBUGFLAGS}>")
target_compile_options(${PROJECT_NAME} PUBLIC "$<$<CONFIG:Release>:${PYTHONFLAGS}>")



if (BUILD_EXECUTABLE)
    add_executable(${PROJECT_NAME}_executable ${PREPENDED_EXECUTABLE_FILES})
    target_link_libraries(${PROJECT_NAME}_executable PRIVATE ${PROJECT_NAME})
    install(TARGETS ${PROJECT_NAME}_executable DESTINATION bin)
    set_target_properties(${PROJECT_NAME}_executable PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIR})
    target_compile_options(${PROJECT_NAME}_executable PUBLIC "$<$<CONFIG:Release>:${OPTIMFLAGS}>")
    target_compile_options(${PROJECT_NAME}_executable PUBLIC "$<$<CONFIG:Parallel>:${OPTIMFLAGS}>")
    target_compile_options(${PROJECT_NAME}_executable PUBLIC "$<$<CONFIG:Parallel>:${MPFLAGS}>")
    target_compile_options(${PROJECT_NAME}_executable PUBLIC "$<$<CONFIG:Dev>:${DEVFLAGS}>")
    target_compile_options(${PROJECT_NAME}_executable PUBLIC "$<$<CONFIG:Debug>:${DEBUGFLAGS}>")
    target_compile_options(${PROJECT_NAME}_executable PUBLIC "$<$<CONFIG:Release>:${PYTHONFLAGS}>")
endif()


# include the build test directory
if(BUILD_TESTS)
  add_subdirectory(test)
endif()

# add coverage compiler flags
if ( ( CMAKE_Fortran_COMPILER MATCHES ".*gfortran.*" OR CMAKE_Fortran_COMPILER MATCHES ".*gcc.*" ) AND 
     ( CMAKE_BUILD_TYPE MATCHES "Debug*" OR CMAKE_BUILD_TYPE MATCHES "Dev*" ) )

    append_coverage_compiler_flags()

    # Copy test data files
    add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/test/test
        COMMAND mkdir -p ${CMAKE_BINARY_DIR}/test/test
    )
    add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/test/test/data
        COMMAND cp -r ${CMAKE_CURRENT_LIST_DIR}/test/data ${CMAKE_BINARY_DIR}/test/test/.
        COMMENT "Copying test data files"
    )
    add_custom_target(copy_test_data ALL
        DEPENDS ${CMAKE_BINARY_DIR}/test/test ${CMAKE_BINARY_DIR}/test/test/data
    )

endif()