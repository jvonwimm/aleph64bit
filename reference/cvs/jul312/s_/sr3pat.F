      SUBROUTINE SR3PAT(ISIDE,IER)
C----------------------------------------------------------------------
C! Find patches of clusters
C!
C!    Author:     H. Meinhard       07-Jan-1988
C!    Modified:   H. Meinhard       07-Aug-1988
C!
C!    Input:      - ISIDE     /I    current side [1,2]
C!    Output:     - IER       /I    error flag (.ne. 0 on error)
C!
C!    Description
C!    ===========
C!    Loop over clusters in all three orientations and look for
C!    areas where the clusters overlap. Dead zones due to sector
C!    borders and gas channels are taken into account.
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "sanamc.h"
#include "sccpjj.h"
#include "sclujj.h"
#include "sgeomc.h"
#include "spatjj.h"
#include "spcpjj.h"
#include "srecpc.h"
#include "ssbpjj.h"
C The patch has a maximum of eight corners. AX = ax, AY = ay
C of corner
      PARAMETER (MXCOR=8)
      REAL AX(MXCOR),AY(MXCOR),AXBAK(MXCOR),AYBAK(MXCOR)
C There are three spacial orientations
      PARAMETER (MOR=3)
      INTEGER ISC(MOR)
      REAL PHI(MOR)
#include "bmacro.h"
#include "smacro.h"
C Statement function: x of intersection points
      XS(T1,T2,P1,P2) = (T1*SIN(P2)-T2*SIN(P1))/SIN(P2-P1)
C Statement function: y of intersection points
      YS(T1,T2,P1,P2) = (T2*COS(P1)-T1*COS(P2))/SIN(P2-P1)
C----------------------------------------------------------------------
C get mean value of SATR z coordinate
      Z = ZZERSG - ZOFFSG - FLOAT(MLAYSG-1)*ZDELSG/2.
C
C link to clusters and patches
      KSCLU = IW(NASCLU)
      KSCCP = IW(NASCCP)
      KSSBP = IW(NASSBP)
      KSPAT = IW(NASPAT)
      KSPCP = IW(NASPCP)
C
C top of loop over clusters in orientation 1
      IND = (ISIDE-1)*3
      DO 370 ICL1 = ITABL(KSSBP,IND+1,JSSBPB),
     +              ITABL(KSSBP,IND+1,JSSBPE)
        ISC(1) = ITABL(KSCLU,ICL1,JSCLSC)
        PHI(1) = PHIXXX(1,ISC(1))
        THL1 = RTABL(KSCLU,ICL1,JSCLTL)
        THU1 = RTABL(KSCLU,ICL1,JSCLTU)
C
C top of loop over clusters in orientation 2
        DO 360 ICL2 = ITABL(KSSBP,IND+2,JSSBPB),
     +                ITABL(KSSBP,IND+2,JSSBPE)
          ISC(2) = ITABL(KSCLU,ICL2,JSCLSC)
          PHI(2) = PHIXXX(2,ISC(2))
          THL2 = RTABL(KSCLU,ICL2,JSCLTL)
          THU2 = RTABL(KSCLU,ICL2,JSCLTU)
C
C get corners of initial parallelogram
          NC = 4
          AX(1) = XS(THL1,THL2,PHI(1),PHI(2))
          AY(1) = YS(THL1,THL2,PHI(1),PHI(2))
          AX(2) = XS(THL1,THU2,PHI(1),PHI(2))
          AY(2) = YS(THL1,THU2,PHI(1),PHI(2))
          AX(3) = XS(THU1,THU2,PHI(1),PHI(2))
          AY(3) = YS(THU1,THU2,PHI(1),PHI(2))
          AX(4) = XS(THU1,THL2,PHI(1),PHI(2))
          AY(4) = YS(THU1,THL2,PHI(1),PHI(2))
C
C clip at sector borders / gas channels
          DO 300 I = 1, 2
            DCUT = DSECSR/Z
            IF (ISC(I).EQ.1 .OR. ISC(I).EQ.5) DCUT = DGASSR/Z
            PH = PHI(I) - PI/FLOAT(MSECSG) + PIBY2
            CALL SRCLIP(NC,AX,AY,DCUT,PH,2)
            IF (NC .EQ. 0)                                  GOTO 360
            DCUT = DSECSR/Z
            IF (ISC(I).EQ.4 .OR. ISC(I).EQ.8) DCUT = DGASSR/Z
            PH = PHI(I) + PI/FLOAT(MSECSG) - PIBY2
            CALL SRCLIP(NC,AX,AY,DCUT,PH,2)
            IF (NC .EQ. 0)                                  GOTO 360
  300     CONTINUE
C
C save polygon since we need it several times
          CALL SRCCOP(NC,AX,AY,NCBAK,AXBAK,AYBAK)
C
C top of loop over clusters in orientation 3
          DO 350 ICL3 = ITABL(KSSBP,IND+3,JSSBPB),
     +                  ITABL(KSSBP,IND+3,JSSBPE)
            ISC(3) = ITABL(KSCLU,ICL3,JSCLSC)
            PHI(3) = PHIXXX(3,ISC(3))
            THL3 = RTABL(KSCLU,ICL3,JSCLTL)
            THU3 = RTABL(KSCLU,ICL3,JSCLTU)
C
C get polygon from backup copy
            CALL SRCCOP(NCBAK,AXBAK,AYBAK,NC,AX,AY)
C
C clip at cluster in third orientation
C if polygon is suppressed completely, take next cluster
            CALL SRCLIP(NC,AX,AY,THL3,PHI(3),2)
            IF (NC .EQ. 0)                                  GOTO 350
            CALL SRCLIP(NC,AX,AY,THU3,PHI(3),1)
            IF (NC .EQ. 0)                                  GOTO 350
C
C clip at sector borders / gas channels
            DCUT = DSECSR/Z
            IF (ISC(3) .EQ. 1 .OR. ISC(3) .EQ. 5) DCUT = DGASSR/Z
            PH = PHI(3) - PI/FLOAT(MSECSG) + PIBY2
            CALL SRCLIP(NC,AX,AY,DCUT,PH,2)
            IF (NC .EQ. 0)                                  GOTO 350
            DCUT = DSECSR/Z
            IF (ISC(3) .EQ. 4 .OR. ISC(3) .EQ. 8) DCUT = DGASSR/Z
            PH = PHI(3) + PI/FLOAT(MSECSG) - PIBY2
            CALL SRCLIP(NC,AX,AY,DCUT,PH,2)
            IF (NC .EQ. 0)                                  GOTO 350
C
C patch still existing. Fill into BOS bank
            IF (LFRROW(KSPAT) .LE. 0)                       GOTO 901
            IW(KNEXT(KSPAT)+JSPASI) = ISIDE
            IW(KNEXT(KSPAT)+JSPANC) = NC
            DO 310 IC = 1, NC
              RW(KNEXT(KSPAT)+JSPAAX+IC-1) = AX(IC)
              RW(KNEXT(KSPAT)+JSPAAY+IC-1) = AY(IC)
  310       CONTINUE
            NI = 0
            DO 320 IWI = 1, ITABL(KSCLU,ICL1,JSCLNW)
              NI = NI+1
              IW(KNEXT(KSPCP)+JSPCPC+NI-1) = ITABL(KSCCP,ICL1,IWI)
  320       CONTINUE
            DO 330 IWI = 1, ITABL(KSCLU,ICL2,JSCLNW)
              NI = NI+1
              IW(KNEXT(KSPCP)+JSPCPC+NI-1) = ITABL(KSCCP,ICL2,IWI)
  330       CONTINUE
            DO 340 IWI = 1, ITABL(KSCLU,ICL3,JSCLNW)
              NI = NI+1
              IW(KNEXT(KSPCP)+JSPCPC+NI-1) = ITABL(KSCCP,ICL3,IWI)
  340       CONTINUE
            IW(KNEXT(KSPAT)+JSPANW) = NI
            IW(KSPAT+LMHROW) = LROWS(KSPAT)+1
            IW(KSPCP+LMHROW) = LROWS(KSPCP)+1
C
C bottom of loop over clusters
  350     CONTINUE
  360   CONTINUE
  370 CONTINUE
C
      GOTO 999
C----------------------------------------------------------------------
C error condition: not enough space in patch banks
  901 IER = 1
      GOTO 999
  999 CONTINUE
      RETURN
      END
#endif
