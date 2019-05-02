<<<<<<< HEAD
rm Makefile
rm CMakeCache.txt
rm -r CMakeFiles
rm cmake_install.cmake
cmake -DGeant4_DIR=/usr/local/GEANT4/geant4.9.6.p04 /home/mwilliams/Documents/EMMA/G4EMMA
=======
rm -rf EMMAapp Makefile CMakeCache.txt CmakeFiles/ cmake_install.cmake

GEANT4_DIR=~/GEANT4/geant4.9.6-install/lib/Geant4-9.6.4/

cmake -g -DGeant4_DIR=/opt/geant4/geant4.9.6.p04-build . 

make -j4
>>>>>>> 328d247d31d8d865b2def4e9637587fef9e7941b
