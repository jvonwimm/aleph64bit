# aleph64bit
Unofficial 64-bit gfortran version of software for the ALEPH experiment at LEP.
Uses 64-bit version of CERNLIB based on the recipe here:
https://www-zeuthen.desy.de/linear_collider/cernlib/new/cernlib_2005.html

# This version (gcc-7) was compiled and tested in Linux ubuntu 18.04

# Prerequisites:
sudo apt-get install tcsh
# check if gmake exists, if not, usual solution is to make a symbolic link:
sudo ln -s /usr/bin/make /usr/bin/gmake
sudo apt-get install gfortran-multilib
sudo apt-get install libfreetype6-dev
sudo apt-get install libncurses-dev
sudo apt-get install xutils-dev
sudo apt-get install libgl1-mesa-dev
sudo apt-get install libxt-dev
sudo apt-get install libmotif-dev

# setup the ALEPH environment
source setaleph.csh

#==================================================
# to compile CERNLIB from source to a new directory
# (for example cerntest)
./installcernlib.csh cerntest
# edit setaleph.csh to change the CERN environment
# variable to point to cerntest, then:
source setaleph.csh
#==================================================

#==================================================
# to (re)-compile the ALEPH software:
# make sure the ALEPH environment is there by doing
# source setaleph.csh if necessary.
# You may wish to save copy of existing executables
# and libraries:   cp -rp Linux Linux_prev
# now remake libraries and executables:
./makealeph.csh
#==================================================
