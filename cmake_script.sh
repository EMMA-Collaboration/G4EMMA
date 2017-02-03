#!/bin/bash
#
# A simple helper script to run the proper cmake for G4Emma

#where we'll build the G4 project
BUILD_DIR="G4EMMA-build"

GEANT4_DIR=/opt/geant4/geant4.9.6.p04

mkdir -p $BUILD_DIR
cd $BUILD_DIR

#remove all the shit that's there before
rm -r *

#CMake stuff is in the above directory
cmake -DGeant4_DIR=$GEANT4_DIR ..

#Assumes you have a quad core processor
make -j4

#move the executable up to the main folder
mv EMMAapp ..


