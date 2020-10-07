#!/bin/tcsh
# BOS77
cd $ALEPH/gen
maklib bos77 3489
ln -sf libbos3489.a libbos77.a
ln -sf libbos3489_dbx.a libbos77_dbx.a
# ALEPHLIB
maklib alephlib 316
maklib alephio 83
mergeale update aleph libalephio316
mergeale update alio libalephio316
ln -sf libalephio316.a libalephlib.a
ln -sf libalephio316_dbx.a libalephlib_dbx.a
# LOOK 
maklib look 25
ln -sf look25 look
ln -sf liblook25.a liblook.a

# GALEPH
cd ../gal
maklib tpcsim 300
${FC} ${FCOPT} -I$CERN_ROOT/include gmults.F
maklib galeph 309
ln -sf libgal309.a libgaleph.a
ln -sf libgal309_dbx.a libgaleph_dbx.a
ln -sf libtpc300.a libtpcsim.a
ln -sf libtpc300_dbx.a libtpcsim_dbx.a
mv -f gal309new gal309
ln -sf gal309 galeph

# JULIA
cd ../jul
maklib julia 313
ln -sf libjul313.a libjulia.a
ln -sf libjul313_dbx.a libjulia_dbx.a
mv -f jul313new jul313
ln -sf jul313 julia

# ALPHA and MINIPROD
cd ../phy
maklib alpha 126
ln -sf libalpha126.a libalpha.a
ln -sf libalpha126_dbx.a libalpha_dbx.a
ln -s $ALEPH_ROOT/reference/phy/qusig.c qusig.c
gcc -c qusig.c
make --always-make -f miniprod.mk

# DALI
cd $ALEPH_ROOT/reference/dali/f2
make
rm -f *.o
mv -f dali_f2.Linux $ALEPH/dali/f2/

# DATABASE tools
cd $ALEPH/dbase
make --always-make NAME=sbank
make --always-make NAME=hacpar
make --always-make NAME=creind
make --always-make NAME=eptoda
make --always-make NAME=chkcrd
make --always-make NAME=cardaf
make --always-make NAME=epiofmt
make --always-make NAME=dafcar
make --always-make NAME=newdaf
make --always-make NAME=upddaf

# Kingal libraries etc
cd $ALEPH_ROOT/reference/kin
make --always-make -f kinagain_203_1000_lx.mk
make --always-make -f kinagain_203_lx.mk
make --always-make -f kinagain_204_lx.mk
# make the YFSWW reweighting executable
make --always-make -f ywrewt_lx.mk
# grace libraries
cd grace200k
make_grace grace200k
# grace quadruple precision
cd ../grace200kquad
make_grace grace200kquad
cd ..
# make the rest of the libraries
makekinlibs

# SCANBOOK
cd $ALEPH/scanbook
make --always-make


