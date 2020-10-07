      SUBROUTINE ECHIBA
C***********************************************************
C! Fill Gammas ecal histograms
C*   Called if histogramm flag = 4
C*   Histo. number 4400 to 4499
C************************************************************
#ifndef DOC
#include "rflags.h"
#include "bcs.h"
#include "rparac.h"
#include "ecnamc.h"
#include "ecobjj.h"
#include "ehypjj.h"
#include "eparjj.h"
#include "bmacro.h"
C
      KECOB = IW(NAECOB)
      IF (KECOB.EQ.0) GO TO 97
      KEHYP = IW(NAEHYP)
      IF (KEHYP.EQ.0) GO TO 97
      KEPAR = IW(NAEPAR)
      NECOB = LROWS(KECOB)
C ----- Stacks contents.
      ST1 = 0.
      ST2 = 0.
      ST3 = 0.
      ENMX = 0.
      NECAL = JULEC * 1000 + 400
C ----- Boucle sur les ECOB
      DO 1 ICOB = 1,NECOB
        NTR = ITABL( KECOB , ICOB , JECOET )
        IF (NTR.NE.0) GO TO 1
C ----- Neutral cluster.
C ----- Raw energy.
        RAW = RTABL( KECOB , ICOB , JECOEG )
C ----- Stacks energy.
        ST1 = ST1 + RTABL( KECOB , ICOB , JECOEN )
        ST2 = ST2 + RTABL( KECOB , ICOB , JECOEN + 1 )
        ST3 = ST3 + RTABL( KECOB , ICOB , JECOEN + 2 )
        IHYP = ITABL( KECOB , ICOB , JECOEH )
        IF( IHYP .EQ. 0 ) GO TO 1
C ----- Hypothesis type.
        ITYP = ITABL( KEHYP , IHYP , JEHYTY )
        TYPE = ITYP
        CALL HFILL( NECAL + 1 , TYPE , 0., 1.)
        IF( ITYP .LT. 64 ) CALL HFILL( NECAL + 2 , TYPE , 0., 1.)
C ----- Corrected energy.
        ENER = RTABL( KEHYP , IHYP , JEHYEN )
C ----- Shape proba.
        CHIL = RTABL( KECOB , ICOB , JECOIT + 1 )
C ----- Mat. depth proba.
        SMGA = RTABL( KECOB , ICOB , JECOIT )
        IF( ITYP .LT. 32 ) THEN
          IF( ITYP .GE. 16 ) THEN
            CALL HFILL( NECAL + 21 , SMGA , 0., 1.)
          ELSE
            CALL HFILL( NECAL + 22 , SMGA , CHIL , 1.)
          ENDIF
        ENDIF
C   More energetic cluster.
        IF (RAW.GT.ENMX) THEN
          ENMX = RAW
C   First associated particle.
          IPAR = ITABL(KEHYP , IHYP , JEHYEP)
          IF(IPAR .NE. 0) THEN
C ----- Compactness.
            SOMX = RTABL( KEPAR , IPAR , JEPAIT )
C   Transverse shape identificator.
            PI0X = RTABL(KEPAR , IPAR , JEPAIT + 1 )
          ELSE
            SOMX = 0.
            PI0X = 0.
          ENDIF
        ENDIF
C ----- Two hypothesis ?
        NEXT = ITABL( KEHYP , IHYP , JEHYEH )
        IF( NEXT .EQ. 0 ) GO TO 1
C ----- Two hypothesis.
        ENR2 = RTABL( KEHYP , NEXT , JEHYEN ) / ENER
        CALL HFILL( NECAL + 15 , ENR2 ,1. , 2.)
    1 CONTINUE
      STOT = ST1 + ST2 + ST3
      CALL HFILL( NECAL + 11 , STOT ,.0 , 1.)
      CALL HFILL( NECAL + 12 , STOT ,.0 , 1.)
      CALL HFILL( NECAL + 13 , ST1 , ST3 , 1.)
      CALL HFILL( NECAL + 14 , ST2 ,.0  , 1.)
      CALL HFILL( NECAL + 31, SOMX , 0., 1.)
      CALL HFILL( NECAL + 32, PI0X , 0., 1.)
   97 CONTINUE
      RETURN
      END
#endif
