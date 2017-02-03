#----------------------------------------------------------------------------
# Setup the project
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
project(EMMA)

#----------------------------------------------------------------------------
# Find Geant4 package, activating all available UI and Vis drivers by default
# You can set WITH_GEANT4_UIVIS to OFF via the command line or ccmake/cmake-gui
# to build a batch mode only executable
#
option(WITH_GEANT4_UIVIS "Build example with Geant4 UI and Vis drivers" ON)
if(WITH_GEANT4_UIVIS)
  find_package(Geant4 REQUIRED ui_all vis_all)
else()
  find_package(Geant4 REQUIRED)
endif()

#----------------------------------------------------------------------------
# Setup Geant4 include directories and compile definitions
# Setup include directory for this project
#
include(${Geant4_USE_FILE})

#----------------------------------------------------------------------------
# Find ROOT (optional package)
#
find_package(ROOT)
if(ROOT_FOUND)
  add_definitions(-DG4ANALYSIS_USE)   
endif()

#----------------------------------------------------------------------------
# Locate sources and headers for this project
# NB: headers are included so they will show up in IDEs
#
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include 
                    ${Geant4_INCLUDE_DIR}
                    ${ROOT_INCLUDE_DIR})

#Grab all source/header files aren't hidden / edits				  
file(GLOB sources "${PROJECT_SOURCE_DIR}/src/[^.]*.cc")
file(GLOB headers "${PROJECT_SOURCE_DIR}/include/[^.]*.hh")


#----------------------------------------------------------------------------
# Find the fortran2c library for mitray
find_library(F2C_LIBRARY
    NAMES libf2c.a
    PATHS 
    /opt/local/lib/
    /usr/local/lib/
    /usr/lib
	[NO_DEFAULT_PATH]
    )
message(STATUS "Lib file detected: [${F2C_LIBRARY}].")

#---------------------------------------------------------------------------
#Make the executable with all the necessary libs
add_executable(EMMAapp EMMAapp.cc ${sources} ${headers})
target_link_libraries(EMMAapp ${Geant4_LIBRARIES} ${ROOT_LIBRARIES} ${F2C_LIBRARY} )

#----------------------------------------------------------------------------
# Copy all scripts to the build directory, i.e. the directory in which we
# build A01. This is so that we can run the executable directly because it
# relies on these scripts being in the current working directory.
#
set(EMMA_SCRIPTS
   BeamSetup.mac visEMMA.mac
  )

foreach(_script ${EMMA_SCRIPTS})
  configure_file(
    ${PROJECT_SOURCE_DIR}/${_script}
    ${PROJECT_BINARY_DIR}/${_script}
    COPYONLY
    )
endforeach()

#----------------------------------------------------------------------------
# Add program to the project targets
# (this avoids the need of typing the program name after make)
#
add_custom_target(EMMA DEPENDS EMMAapp)

#----------------------------------------------------------------------------
# Install the executable to 'bin' directory under CMAKE_INSTALL_PREFIX
#
install(TARGETS EMMAapp DESTINATION bin)

