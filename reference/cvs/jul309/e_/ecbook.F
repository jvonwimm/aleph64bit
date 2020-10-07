      SUBROUTINE ECBOOK
C----------------------------------------------------------------------
C!  - Book ecal histograms
C!   Book hitograms to verify
C!      the behaviour of ecal recons
C!
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
C
      NECAL = JULEC * 1000
      CALL HBOOK2(NECAL+901,'ECAL ELEC IDENTIF P VX E4/P$',100,0.,2.
     &,50,0.,40.,0.)
      CALL HBOOK1(NECAL+902,'ECAL ELEC IDENTIF R2 ESTIMATOR$',100,-20.
     &,20.,0.)
      IF(JHISRF(JULEC).EQ.5) THEN
C
C *** Book histograms for corrections for photons in crack regions.
C
         CALL HBOOK1(NECAL + 451 ,
     +   'ECAL : TRUE CRACK ENERGY DIFF$'
     +   ,100,-5.,5.,0.)
         CALL HBOOK1(NECAL + 452 ,
     +   'ECAL : TRUE CRACK PHI DIFF$'
     +   ,100,-0.01,0.01,0.)
         CALL HBOOK1(NECAL + 453 ,
     +   'ECAL : MODULE ENERGY DIFF$'
     +   ,100,-5.,5.,0.)
         CALL HBOOK1(NECAL + 454 ,
     +   'ECAL : MODULE PHI DIFF$'
     +   ,100,-0.01,0.01,0.)
         CALL HBOOK1(NECAL + 455, 'ECAL : CRACK RATIO1    $'
     +   ,50,0.,1.,0.)
         CALL HBOOK1(NECAL + 456, 'ECAL : CRACK RATIO2    $'
     +   ,50,0.,1.,0.)
         CALL HBOOK1(NECAL + 457, 'ECAL : CRACK PHI CORRN DEAD REG$'
     +   ,100,-0.02,0.02,0.)
         CALL HBOOK1(NECAL + 458, 'ECAL : CRACK PHI CORRN LIVE REG$'
     +   ,100,-0.02,0.02,0.)
         CALL HBOOK1(NECAL + 459, 'ECAL : CRACK ENE CORRN DEAD REG$'
     +   ,100,-1.0,19.0,0.)
         CALL HBOOK1(NECAL + 460, 'ECAL : CRACK ENE CORRN LIVE REG$'
     +   ,100,-1.0,1.0,0.)
      ENDIF
      CALL HBOOK2(NECAL+903,'ECAL ELEC ID E4 STK3 VS (E4STK1+EASTK2)',
     &            50,0.,50.,50,0.,50.,0.)
      CALL HBOOK1(NECAL+1,'ECAL- STOREYS NUMBER$',50,0.,0.,0.)
      CALL HBOOK1(NECAL+2,'ECAL- STOREYS ENERGY$',50,0.,0.,0.)
      CALL HBOOK1(NECAL+3,'ECAL- CLUSTERS NUMBER$',50,1.,0.,0.)
      CALL HBOOK1(NECAL+4,'ECAL- CLUSTERS ENERGY$',50,0.,0.,0.)
      CALL HBOOK1(NECAL+5,'ENERGY TRACKS ASSOCIATED EC_CLUSTERS$',
     *            50, 0.,0.,0.)
C
C ----- Gammas histograms.
         IF (JHISRF(JULEC).EQ.4) CALL ECBOBA
C? ALIGNEMENT HISTOGRAMS
      IF (JHISRF(JULEC).EQ.3) THEN
        DO 10 I=1,32
          CALL HBOOK1(NECAL+300+I,'Ecal Alignement:TOTAL DIFF TR-ECAL$',
     1                      50,-2.5,2.5,0.)
C? PROJECTION BOOKING (NEED LOT OF SPACE!)
C      CALL HBOOK1(NECAL+332+I,'Ecal Alignement:Xproj DIFF TR-ECAL$',
C    1                      50,-2.5,2.5,0.)
C      CALL HBOOK1(NECAL+364+I,'Ecal Alignement:Yproj DIFF TR-ECAL$',
C     1                      50,-2.5,2.5,0.)
C      CALL HBOOK1(NECAL+396+I,'Ecal Alignement:Zproj DIFF TR-ECAL$',
C     1                      50,-2.5,2.5,0.)
 10     CONTINUE
      ENDIF
      END
#endif
