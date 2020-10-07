      SUBROUTINE SR2PAT(ISIDE,IOR1,IOR2,IOR3,N3TOT,IER)
C----------------------------------------------------------------------
C! Find patches of clusters
C!
C!    Author:     H. Meinhard       12-Jan-1988
C!    Modified:   H. Meinhard       02-Mar-1990  (2)
C!
C!    Input:      - ISIDE     /I    current side [1,2]
C!                - IOR1,2    /I    orientations from which clusters
C!                                  should be considered [1,3]
C!                - IOR3      /I    orientations from which single hits
C!                                  should be considered [1,3]
C!                - N3TOT     /I    number of patches found, without
C!                                  actual 2-patches [0,*]
C!    Output:     - IER       /I    error flag (.ne. 0 on error)
C!
C!    Description
C!    ===========
C!    Look for patches consisting of two clusters and optionally an
C!    additional wire hit in the third orientation. Use this patch only
C!    if it is not a subset of a patch consisting of three clusters.
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "sanamc.h"
#include "sccpjj.h"
#include "sclujj.h"
#include "scoojj.h"
#include "sgeomc.h"
#include "spatjj.h"
#include "spcpjj.h"
#include "srecpc.h"
#include "ssbpjj.h"
#include "sscpjj.h"
      PARAMETER (MXCOR=8)
      REAL AX(MXCOR),AY(MXCOR),AXBAK(MXCOR),AYBAK(MXCOR)
      PARAMETER (MOR=3)
      INTEGER ISC(MOR)
      REAL PHI(MOR)
      LOGICAL LSING,SRSUBS
      INTEGER IARR(MLASG)
      EXTERNAL SRSUBS
#include "bmacro.h"
#include "smacro.h"
C Statement function: x of intersection points
      XS(T1,T2,P1,P2) = (T1*SIN(P2)-T2*SIN(P1))/SIN(P2-P1)
C Statement function: y of intersection points
      YS(T1,T2,P1,P2) = (T2*COS(P1)-T1*COS(P2))/SIN(P2-P1)
C----------------------------------------------------------------------
C get half width of tubes in tan(theta)
      Z = ZZERSG - ZOFFSG - FLOAT(MLAYSG-1)*ZDELSG/2.
      DTBY2 = ZONWSR/(2.*Z)
C
C link to coordinates, clusters and patches
      KSCOO = IW(NASCOO)
      KSSCP = IW(NASSCP)
      KSCLU = IW(NASCLU)
      KSCCP = IW(NASCCP)
      KSSBP = IW(NASSBP)
      KSPAT = IW(NASPAT)
      KSPCP = IW(NASPCP)
C
C top of loop over clusters in orientation IOR1
      IND = (ISIDE-1)*3
      DO 410 ICL1 = ITABL(KSSBP,IND+IOR1,JSSBPB),
     +              ITABL(KSSBP,IND+IOR1,JSSBPE)
        ISC(1) = ITABL(KSCLU,ICL1,JSCLSC)
        PHI(1) = PHIXXX(IOR1,ISC(1))
        THL1 = RTABL(KSCLU,ICL1,JSCLTL)
        THU1 = RTABL(KSCLU,ICL1,JSCLTU)
        NW1 = ITABL(KSCLU,ICL1,JSCLNW)
C
C top of loop over clusters in orientation IOR2
        DO 400 ICL2 = ITABL(KSSBP,IND+IOR2,JSSBPB),
     +                ITABL(KSSBP,IND+IOR2,JSSBPE)
          ISC(2) = ITABL(KSCLU,ICL2,JSCLSC)
          PHI(2) = PHIXXX(IOR2,ISC(2))
          THL2 = RTABL(KSCLU,ICL2,JSCLTL)
          THU2 = RTABL(KSCLU,ICL2,JSCLTU)
          NW2 = ITABL(KSCLU,ICL2,JSCLNW)
C
C LSING will be set to .TRUE. if patch with single hit found
          LSING = .FALSE.
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
            IF (NC .EQ. 0)                                  GOTO 400
            DCUT = DSECSR/Z
            IF (ISC(I).EQ.4 .OR. ISC(I).EQ.8) DCUT = DGASSR/Z
            PH = PHI(I) + PI/FLOAT(MSECSG) - PIBY2
            CALL SRCLIP(NC,AX,AY,DCUT,PH,2)
            IF (NC .EQ. 0)                                  GOTO 400
  300     CONTINUE
C
C save polygon since we need it several times
          CALL SRCCOP(NC,AX,AY,NCBAK,AXBAK,AYBAK)
C
C top of loop over hits in orientation IOR3
          DO 350 IHT3 = ITABL(KSSCP,(IND+IOR3-1)*3+1,JSSCPB),
     +                  ITABL(KSSCP,(IND+IOR3-1)*3+3,JSSCPE)
            ILA3 = ITABL(KSCOO,IHT3,JSCOLA)
            ISC(3) = ITABL(KSCOO,IHT3,JSCOSC)
            PHI(3) = PHIXXX(ILA3,ISC(3))
            TT3 =  (RTABL(KSCOO,IHT3,JSCOTT)+RTABL(KSCOO,IHT3,
     +             JSCOTT+1))/2.
C
C get polygon from backup copy
            CALL SRCCOP(NCBAK,AXBAK,AYBAK,NC,AX,AY)
C
C clip at hit in orientation IOR3
            CALL SRCLIP(NC,AX,AY,TT3-DTBY2,PHI(3),2)
            IF (NC .EQ. 0)                                  GOTO 350
            CALL SRCLIP(NC,AX,AY,TT3+DTBY2,PHI(3),1)
            IF (NC .EQ. 0)                                  GOTO 350
C
C clip at sector borders / gas channels
            DCUT = DSECSR/Z
            IF (ISC(3).EQ.1 .OR. ISC(3).EQ.5) DCUT = DGASSR/Z
            PH = PHI(3) - PI/FLOAT(MSECSG) + PIBY2
            CALL SRCLIP(NC,AX,AY,DCUT,PH,2)
            IF (NC .EQ. 0)                                  GOTO 350
            DCUT = DSECSR/Z
            IF (ISC(3).EQ.4 .OR. ISC(3).EQ.8) DCUT = DGASSR/Z
            PH = PHI(3) + PI/FLOAT(MSECSG) - PIBY2
            CALL SRCLIP(NC,AX,AY,DCUT,PH,2)
            IF (NC .EQ. 0)                                  GOTO 350
C
C candidate for patch found. Look whether subset of 3-patch
C IARR contains the hit numbers
            DO 310 I = 1, NW1
  310       IARR(I) = ITABL(KSCCP,ICL1,JSCCCC+I-1)
            DO 320 I = 1, NW2
  320       IARR(NW1+I) = ITABL(KSCCP,ICL2,JSCCCC+I-1)
            IARR(NW1+NW2+1) = IHT3
            CALL INTSOR(IARR,NW1+NW2+1)
            IF (.NOT. SRSUBS(ISIDE,N3TOT,IARR,NW1+NW2+1)) THEN
C
C patch candidate is no subset of 3-patch; store it
              LSING = .TRUE.
              IF (LFRROW(KSPAT) .LE. 0)                     GOTO 901
              IW(KNEXT(KSPAT)+JSPASI) = ISIDE
              IW(KNEXT(KSPAT)+JSPANC) = NC
              IW(KNEXT(KSPAT)+JSPANW) = NW1+NW2+1
              DO 330 IC = 1, NC
                RW(KNEXT(KSPAT)+JSPAAX+IC-1) = AX(IC)
                RW(KNEXT(KSPAT)+JSPAAY+IC-1) = AY(IC)
  330         CONTINUE
              DO 340 IWI = 1, NW1+NW2+1
  340         IW(KNEXT(KSPCP)+JSPCPC+IWI-1) = IARR(IWI)
              IW(KSPAT+LMHROW) = LROWS(KSPAT)+1
              IW(KSPCP+LMHROW) = LROWS(KSPCP)+1
            ENDIF
C
C bottom of loop over hits in orientation IOR3
  350     CONTINUE
C
C if no patch of two clusters and one single hit found, look whether
C two clusters are subset of a 3-patch; if not, store 2-patch
          IF (.NOT. LSING) THEN
            DO 360 I = 1, NW1
  360       IARR(I) = ITABL(KSCCP,ICL1,JSCCCC+I-1)
            DO 370 I = 1, NW2
  370       IARR(NW1+I) = ITABL(KSCCP,ICL2,JSCCCC+I-1)
            CALL INTSOR(IARR,NW1+NW2)
            IF (.NOT. SRSUBS(ISIDE,N3TOT,IARR,NW1+NW2+1)) THEN
              IF (LFRROW(KSPAT) .LE. 0)                     GOTO 901
              IW(KNEXT(KSPAT)+JSPASI) = ISIDE
              IW(KNEXT(KSPAT)+JSPANC) = NCBAK
              IW(KNEXT(KSPAT)+JSPANW) = NW1+NW2
              DO 380 IC = 1, NCBAK
                RW(KNEXT(KSPAT)+JSPAAX+IC-1) = AXBAK(IC)
                RW(KNEXT(KSPAT)+JSPAAY+IC-1) = AYBAK(IC)
  380         CONTINUE
              DO 390 IWI = 1, NW1+NW2
  390         IW(KNEXT(KSPCP)+JSPCPC+IWI-1) = IARR(IWI)
              IW(KSPAT+LMHROW) = LROWS(KSPAT)+1
              IW(KSPCP+LMHROW) = LROWS(KSPCP)+1
            ENDIF
          ENDIF
C
C bottom of loop over clusters in orientation IOR2
  400   CONTINUE
C
C bottom of loop over clusters in orientation IOR1
  410 CONTINUE
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
