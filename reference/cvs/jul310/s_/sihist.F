      SUBROUTINE SIHIST
C----------------------------------------------------------------------
C!  - Fill SCAL histograms
C!
C!
C!   Author   : B.Bloch-Devaux 911029
C!
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rlunit.h"
#include "rflags.h"
#include "sinamc.h"
#include "spdajj.h"
#include "sbhajj.h"
#include "sclsjj.h"
#include "silujj.h"
#include "silhjj.h"
#include "alcons.h"
      DIMENSION MASKM(3)
      DIMENSION EPAD(2),P1(3),P2(3),EPLA(12),NPAD(12,2),ERAD(16,12)
      EXTERNAL NAMIND
      DATA MASKM/127,1,2/
#include "bmacro.h"
C---------------------------------------------------------------------
      LHID = JULSI*1000
      LHID0= JULSI*1000
      KSCLU = IW(NASCLS)
      KSILU = IW(NASILU)
      KSILH = IW(NASILH)
      KSREC = IW(NASREC)
      KSPDA = IW(NASPDA)
C
C Event weight
      W = 1.
      KKEVH = IW(NAMIND('KEVH'))
      IF(KKEVH.GT.0) W = RTABL(KKEVH,1,5)
      IF(KSILU.GT.0)  THEN
C
C? Histogram events with Scal clusters for general monitoring
C
        NCLU = LROWS(KSILU)
        IF ( NCLU.LE.0) GO TO 999
        DO 17 IMETH = 1 ,3
          METHW = 64
          IF ( KSILH.GT.0)METHW = ITABL(KSILH,1,JSILAM)
          IF ( METHW.EQ.0) METHW= 64
          IF ( IAND(METHW,MASKM(IMETH)).NE.0) THEN
            LHID = LHID0+100*(IMETH-1)
            E1 = 0.
            E2 = 0.
            E3 = 0.
            E4 = 0.
            RNPAC1= 0.
            RNPAC2= 0.
            RNPAC3= 0.
            RNPAC4= 0.
            IF ( NCLU.EQ.1) THEN
              IF ( RTABL(KSILU,1,JSILZC).GT.0.) THEN
                E1 = RTABL(KSILU,1,JSILEC)
                E3 = RTABL(KSILU,1,JSILES)
                RNPAC1= FLOAT ( ITABL(KSILU,1,JSILNP) )
                RNPAC3= FLOAT ( ITABL(KSILU,1,JSILNS) )
              ELSE IF ( RTABL(KSILU,1,JSILZC).LT.0.) THEN
                E2 = RTABL(KSILU,1,JSILEC)
                E4 = RTABL(KSILU,1,JSILES)
                RNPAC2= FLOAT ( ITABL(KSILU,1,JSILNP) )
                RNPAC4= FLOAT ( ITABL(KSILU,1,JSILNS) )
              ENDIF
            ELSE IF ( NCLU.EQ.2) THEN
              E1 = RTABL(KSILU,1,JSILEC)
              E2 = RTABL(KSILU,2,JSILEC)
              E3 = RTABL(KSILU,1,JSILES)
              E4 = RTABL(KSILU,2,JSILES)
              RNPAC1= FLOAT ( ITABL(KSILU,1,JSILNP) )
              RNPAC3= FLOAT ( ITABL(KSILU,1,JSILNS) )
              RNPAC2= FLOAT ( ITABL(KSILU,2,JSILNP) )
              RNPAC4= FLOAT ( ITABL(KSILU,2,JSILNS) )
            ENDIF
            RNPAC = RNPAC1 + RNPAC2 + RNPAC3 + RNPAC4
            CALL HF1(LHID+1,E1+E2,W)
            CALL HF1(LHID+16,(E1+E2)/91.268,W)
            CALL HFILL(LHID+12,RNPAC,DUM,W)
            CALL HFILL(LHID+8,E1,E2,W)
            CALL HF1(LHID+11,FLOAT(NCLU),W)
            IF ( E1.GT.0.) CALL HFILL(LHID+2,RNPAC1,E1,W)
            IF ( E2.GT.0.) CALL HFILL(LHID+2,RNPAC2,E2,W)
            IF ( E3.GT.0.) CALL HFILL(LHID+2,RNPAC3,E3,W)
            IF ( E4.GT.0.) CALL HFILL(LHID+2,RNPAC4,E4,W)
            DO 20 I = 1, NCLU
              ECLUS = RTABL(KSILU,I,JSILEC)
              RCLUS = RTABL(KSILU,I,JSILCR)
              ZCLUS = RTABL(KSILU,I,JSILZC)
              PCLUS = RTABL(KSILU,I,JSILCP)
              CALL HF1(LHID+20*(I-1)+4,RCLUS,W)
              CALL HF1(LHID+5+20*(I-1),ABS(ZCLUS),W)
              CALL HF1(LHID+6+20*(I-1),PCLUS,W)
              X = RCLUS*COS(PCLUS)
              Y = RCLUS*SIN(PCLUS)
              CALL HFILL(LHID+31+20*(I-1),X,Y,W)
              CALL HFILL(LHID+ 7+20*(I-1),RCLUS,ECLUS,W)
              CALL HFILL(LHID+ 9+20*(I-1),PCLUS*RADEG,ECLUS,W)
              DO 6 IPL=1,12
                EP = RTABL(KSILU,I,JSILEF+IPL-1)
    6         CALL HFILL(LHID+3+20*(I-1),FLOAT(IPL),DUM,EP)
   20       CONTINUE
          ENDIF
   17   CONTINUE
      ENDIF
C
  999 RETURN
      END
#endif
