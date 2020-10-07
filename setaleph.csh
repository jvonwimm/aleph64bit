#!/bin/tcsh
setenv ALEPH_ROOT $PWD
setenv ALEPH $ALEPH_ROOT/Linux
setenv CERN $ALEPH_ROOT/cern
setenv CERN_LEVEL 2005
setenv CERN_ROOT $CERN/$CERN_LEVEL
setenv MASTER reference
setenv ALROOT $ALEPH_ROOT/$MASTER/cvs

set path = (. $ALEPH/bin $ALEPH/script $CERN_ROOT/bin $path)

setenv propath "$ALEPH/lib $CERN_ROOT"
setenv devpath "$propath"

setenv SHARED $ALEPH_ROOT/shared
setenv ADBSCONS $ALEPH/dbase/adbscons.daf
setenv ADBSTEST $ALEPH/dbase/adbstest.daf
setenv ADBS8990 $ALEPH/dbase/adbs8990.daf
setenv PHYINC $ALEPH/phy
setenv ALEDIR $ALEPH/edir
setenv ALDOC $ALEPH/doc
setenv BANKALFMT $ALEPH/dbase/bankal.fmt
setenv DBASBANK $ALEPH/dbase/dbas.bank
setenv ALPHACARDS $ALEPH/phy/alpha.cards
setenv GALEPHCARDS $ALEPH/gal/galeph.cards
setenv JULIACARDS $ALEPH/jul/julia.cards
setenv KINGALCARDS $ALEPH/kin/kingal.cards
setenv RUNCARTSLIST $ALEPH/book/runcarts.list
setenv LOOKCARDS $ALEPH/gen/look.cards
setenv ALBOOK $ALEPH/book
setenv SCWORK $ALEPH/scwork
setenv BEAMPOSITION $ALEPH/dbase/beam.position
setenv ALSTOUT aldataout
setenv DPMSIZE 200
setenv DPMUSER aleph
setenv STAGELABEL sl
setenv STAGEFSEQ 1
setenv STAGE_HOST shift9
setenv STAGE_USER aleph
setenv STAGERECFM F
setenv STAGELRECL 32040
setenv STAGEBLKSIZ 32040
setenv STAGECLEAN $ALEPH/script/stage-clean-sh
setenv RETRYCOUNT 10
#  for ADAMO
setenv GENATTR $ALEPH/dbase/genattr.ddl
setenv BOSKEY $ALEPH/dbase/boskey.ddl
setenv ALDATA $ALEPH/data
setenv ALSTAGE $ALEPH/data
setenv DPMLINKDIR $ALEPH/data
setenv CVSROOT /al/$MASTER/cvsmaster
setenv CVSIGNORE 'HPUX9 IRIX5 OSF1 fort.1 garbage spy *.i'
setenv ALROOT $ALEPH_ROOT/$MASTER/cvs
setenv TAPESCONF $ALEPH/etc/tapes.conf
setenv ALINC $ALROOT/inc

setenv OS Linux
setenv CPPOPT "-DUNIX -DALEPH_LINUX -DALEPH_GFORT -I${ALINC}"
./getcompiler.sh
setenv FC "`head -1 compversion`"
rm -f compversion
setenv FCOPT  "-c -O -std=legacy -fno-automatic -fdollar-ok -fno-backslash $CPPOPT"
setenv CCOPT "-c -O -DALEPH_C -I${ALINC}"
setenv GUSER "-Wl,-u,qnext_,-u,gpghei_,-u,grndm_,-u,gstmed_,-u,gufld_,-u,guhadr_,-u,gntube_,-u,guphad_,-u,gusear_,-u,gustep_,-u,gutrak_,-u,gdray_"
setenv JMUID "-Wl,-u,ftrack_,-u,hmfind_,-u,mpredg_,-u,mpredm_,-u,muass_"
setenv UNDEF "-Wl,-u,aboldr_,-u,babend_,-u,dafrds_"

setenv ALPVER `egrep -i alpha $ALEPH/doc/progl.lis | awk '{print $2}'`
setenv GALVER `egrep -i galeph $ALEPH/doc/progl.lis | awk '{print $2}'`
setenv JULVER `egrep -i julia $ALEPH/doc/progl.lis | awk '{print $2}'`

