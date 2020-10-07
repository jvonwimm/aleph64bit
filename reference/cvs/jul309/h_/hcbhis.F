      SUBROUTINE HCBHIS
C-----------------------------------------------------------
C!  Book Hcal Histograms
C
C
C!         called by RBKHIS
C
C             G.Capon   date : 870114
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
C
C?             Histograms for analog readout
C
      JOFF = JULHC * 1000
      CALL HBOOK1(JOFF+1,'HCAL - STOREYS/EVENT      $',50,0.,100.,0.)
      CALL HBOOK1(JOFF+2,'HCAL - STOREY ENERGY      $',50,0., 50.,0.)
      CALL HBOOK1(JOFF+3,'HCAL - TOTAL ENERGY       $',50,0.,100.,0.)
      CALL HBOOK1(JOFF+13,'HCAL - TOT. EN. CLEAN EVTS',50,0.,100.,0.)
      CALL HBOOK1(JOFF+4,'HCAL - CLUSTERS/EVENT     $',50,0., 50.,0.)
      CALL HBOOK1(JOFF+5,'HCAL - STOREYS/CLUSTER    $',50,0., 50.,0.)
      CALL HBOOK1(JOFF+15,'HCAL - ST./CLUS. CLEAN CLUS',50,0., 50.,0.)
      CALL HBOOK1(JOFF+6,'HCAL - CLUSTER ENERGY     $',50,0., 50.,0.)
      CALL HBOOK1(JOFF+10,'HCAL - NOISE FLAG',128,0.,128.,0.)
C
C?             Histograms for digital readout
C
      CALL HBOOK1(JOFF+51,'HCAL - WIRE CLUSTERS/EVENT$',50,0.,200.,0.)
      CALL HBOOK1(JOFF+52,'HCAL - LAYER NUMBER       $',25,1., 26.,0.)
      CALL HBOOK1(JOFF+53,'HCAL - WIRES/CLUSTER      $',50,1., 51.,0.)
      CALL HBOOK1(JOFF+54,'HCAL - LAST LAYER         $',25,1., 26.,0.)
C
C              Histograms for Hcal to muon track association
C
      CALL HBOOK1(JOFF+101,'MU-ROAD GEOMETRY FLAG           $'
     *          ,10,0.,10.,0.)
      CALL HBOOK1(JOFF+102,'MU-ROAD CLASSIF   FLAG          $'
     *          ,60,0.,60.,0.)
      CALL HBOOK1(JOFF+103,'MU-ROAD (NHITS- FIRED PLANES)    $'
     *          ,60,0.,60.,0.)
      CALL HBOOK1(JOFF+104,'MU-ROAD (EXPECTED-FIRED) PLANES   $'
     *          ,60,0.,60.,0.)
      CALL HBOOK1(JOFF+105,'MU-ROAD EXPECTED PLANES         $'
     *          ,60,0.,60.,0.)
      CALL HBOOK1(JOFF+106,'MU-ROAD FIRED PLANES IN LAST 10 $'
     *          ,60,0.,60.,0.)
      CALL HBOOK1(JOFF+107,'MU-ROAD HIT MOLTEP IN LAST 10 PL$'
     *          ,60,0.,60.,0.)
      CALL HBOOK1(JOFF+108,'MU-ROAD FIRED PLANES IN ROAD    $'
     *          ,60,0.,60.,0.)
      CALL HBOOK1(JOFF+120,'MU-ROAD (XTREX-XLOC)/SIGMA   $'
     *          ,100,-10.,10.,0.)
C
C  Histograms for HCAL Digital Patterns
C
      CALL HTABLE(JOFF+201, ' HDGFIL: NHCLU VS NPATT$'
     &            ,12,0.,12.,20,0.,20.,9999.)
      CALL HBPRO(JOFF+201,0.)
      CALL HTABLE(JOFF+202, ' HDGFIL: NECLU VS NPATT$'
     &            ,12,0.,12.,50,0.,50.,9999.)
      CALL HBPRO(JOFF+202,0.)
      CALL HBOOK2(JOFF+203,' HDGFIL: TOTAL ENERGY: HCLU VS PATT$'
     &                ,40,0.,120.,40,0.,120.,0.)
      CALL HBOOK1(JOFF+204,' HDGFIL: TOTAL ENERGY: HCLU - PATT$'
     &                ,50,-20.,20.,0.)
      CALL HBOOK1(JOFF+205,' HDGFIL: FIRST PLANE$',24,0.,24.,0.)
      CALL HBOOK1(JOFF+206,' HDGFIL: LAST  PLANE$',24,0.,24.,0.)
      CALL HBOOK1(JOFF+207,' HDGFIL: INTER. PLANE$',24,0.,24.,0.)
      CALL HBOOK1(JOFF+208,' HDGFIL: INTPL - FPLANE$',25,0.,25.,0.)
      CALL HBOOK1(JOFF+209,' HDGFIL: NHIT/PATTERN$',50,0.,100.,0.)
      CALL HBOOK1(JOFF+210,' HDGFIL: TOTAL NUMBER OF HIT$'
     &             ,50,0.,150.,0.)
      CALL HBOOK1(JOFF+211,' HDGFIL: DIRECTION INDEX$',10,-2.,8.,0.)
C
C Histograms for Superpatterns and Extended Calobjects (CEXT)
C H Superpatterns = Objects containg Hcal towers + Hcal tubes
C CEXT = H Superpattern + Ecalob (Through CRL3 relations )
C
      CALL HBOOK1(JOFF+311,' CEXHIS: NUMBER OF H SUPERPATTERNS/EVENT$'
     &           ,20,0.,20.,0.)
      CALL HBOOK1(JOFF+312,' CEXHIS: NUMBER OF PATT/CEXT$'
     &           ,20,0.,20.,0.)
      CALL HBOOK1(JOFF+313,' CEXHIS: NUMBER OF ECLU/CEXT$'
     &           ,20,0.,20.,0.)
      CALL HBOOK1(JOFF+314,' CEXHIS: NUMBER OF HCLU/CEXT$'
     &           ,20,0.,20.,0.)
      CALL HBOOK1(JOFF+315,' CEXHIS: NUMBER OF NEUTRAL CEXT/EVT$'
     &            ,20,0.,20.,0.)
      CALL HTABLE(JOFF+316,' CEXHIS: NECLU VS NPATT$'
     &            ,8,0.,8.,24,0.,24.,0.)
      CALL HTABLE(JOFF+317,' CEXHIS: NHCLU VS NPATT$'
     &            ,6,0.,6.,12,0.,12.,0.)
      DO 200 I=201,211
      CALL HIDOPT(JOFF+I,'1EVL')
 200  CONTINUE
      DO 300 I=311,317
      CALL HIDOPT(JOFF+I,'1EVL')
 300  CONTINUE
      RETURN
      END
#endif
