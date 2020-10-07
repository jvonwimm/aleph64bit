#! /bin/tcsh
if ( $1 == '') then
 set cerndir=cern
else
 set cerndir=`echo $1`
endif
echo Installing cernlib into directory $cerndir
mkdir $cerndir
tar xf cernlib2005.tar -C $cerndir
cd $cerndir
./INSTALL
cd ..
