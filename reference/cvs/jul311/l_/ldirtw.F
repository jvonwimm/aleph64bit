      SUBROUTINE LDIRTW(THETA,PHI,IADDR,DIST)
C-----------------------------------------------------------------------
C! Extrapolate track to Lcal reference plane
C!
C!   Author   : P. H. Hansen   880418
C!   Input    : THETA, PHI    track direction
C!   Output   : IADDR         tower address
C!              DIST(2)       Hor/Ver distance from inner edge (signed)
C!
C!   Description :
C!   ===========
C?      Extrapolate to reference plane (average shower max)
C?      Make alignment corrections and find local coordinates
C?      Test if these are inside the Lcal acceptance + 1cm
C?      Find the corresponding tower address
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "lcnamc.h"
#include "lcrejj.h"
#include "lalijj.h"
#include "lslojj.h"
#include "lmtyjj.h"
#include "lwrgjj.h"
#include "lypars.h"
#include "llayjj.h"
      DIMENSION XTRG(3),DTRG(3),XTRL(3)
      DIMENSION DIST(*)
#include "bmacro.h"
C----------------------------------------------------------------------
      IADDR = 0
C Geometry banks
      KLALI = IW(NALALI)
      KLSLO = IW(NALSLO)
      KLMTY = IW(NALMTY)
      KLWRG = IW(NALWRG)
      KLLAY = IW(NALLAY)
      KLCRE = IW(NALCRE)
C
C Direction in global frame
      DTRG(1) = SIN(THETA)*COS(PHI)
      DTRG(2) = SIN(THETA)*SIN(PHI)
      DTRG(3) = COS(THETA)
C
C Module number
      MODU = 1
      IF(DTRG(1).GT.0.) MODU = 2
      IF(DTRG(3).GT.0.) MODU = MODU + 2
C
C Position in global coordinates
      XTRG(3) = RTABL(KLCRE,1,JLCRZC)*SIGN(1.,DTRG(3))
      XTRG(1) = XTRG(3)*DTRG(1)/DTRG(3)
      XTRG(2) = XTRG(3)*DTRG(2)/DTRG(3)
C
C and in local coordinates
      XTRL(3) = XTRG(3) - RTABL(KLSLO,MODU,JLSLXS+2)
     &                  - RTABL(KLALI,MODU,JLALDX+2)
      XTRL(1) = XTRG(1) - RTABL(KLALI,MODU,JLALDX)
     &                  - RTABL(KLALI,MODU,JLALDR+1)*XTRL(3)
     &                  + RTABL(KLALI,MODU,JLALDR+2)*XTRG(2)
      XTRL(2) = XTRG(2) - RTABL(KLALI,MODU,JLALDX+1)
     &                  - RTABL(KLALI,MODU,JLALDR+2)*XTRG(1)
     &                  + RTABL(KLALI,MODU,JLALDR)*XTRL(3)
C
C Test Lcal acceptance:
C
C   The wire support covers in each end
      DY1 = RTABL(KLMTY,1,JLMTDT)
C
C   Impact point in wire plane quadrant
      X = ABS(XTRL(1))
      Y = ABS(XTRL(2))
C
C   Find distance from inter-model crack
      DIST(1) = X - RTABL(KLWRG,1,JLWRXL)
      DIST(2) = 24.5
C
C   Find distance to "staircase" boundary
C   - approximate it by straight line: Y = YEDGLC - TEDGLC*X
      IF(Y.GT.RTABL(KLWRG,5,JLWRYL)+DY1) THEN
         DIS = X - (YEDGLC-Y)/TEDGLC
         IF(DIS.LT.DIST(1)) DIST(1)=DIS
         DIST(2) = Y - (YEDGLC-X*TEDGLC)
C
C or distance to inner vertical boundary
      ELSE
         DIST(1) = X - RTABL(KLWRG,6,JLWRXL)
      ENDIF
C
C Determine pad size
      SIZE = RTABL(KLCRE,1,JLCRSR)
C
C Allow one pad size outside acceptance
      IF(DIST(1).LT.-SIZE.OR.DIST(2).LT.-SIZE)              GOTO 999
C
C Determine row and column
      IY = MIN0(15,INT((Y-RTABL(KLMTY,1,JLMTCC))/SIZE)+1)
      IX = MIN0(16,INT(X/SIZE+.5))
      IF(IX.LE.0) IX=1
      IF(IY.LE.0) IY=1
C
      IF(IY.GE.1.AND.IY.LE.3) THEN
         IX = MAX0(4,IX)
         GOTO 30
      ENDIF
C
      IF(IY.EQ.4) THEN
         IX = MAX0(3,IX)
         GOTO 30
      ENDIF
C
      IF(IY.EQ.5) THEN
         IX = MAX0(2,IX)
         GOTO 30
      ENDIF
C
      IF(IY.GE.6.AND.IY.LE.8) THEN
         IX = MIN0(15,IX)
         GOTO 30
      ENDIF
C
      IF(IY.EQ.9) THEN
         IX = MIN0(14,IX)
         GOTO 30
      ENDIF
C
      IF(IY.EQ.10.OR.IY.EQ.11) THEN
         IX = MIN0(13,IX)
         GOTO 30
      ENDIF
C
      IF(IY.EQ.12) THEN
         IX = MIN0(12,IX)
         GOTO 30
      ENDIF
C
      IF(IY.EQ.13) THEN
         IX = MIN0(11,IX)
         GOTO 30
      ENDIF
C
      IF(IY.EQ.14) THEN
         IX = MIN0(10,IX)
         GOTO 30
      ENDIF
C
      IF(IY.EQ.15) THEN
         IX = MIN0(8,IX)
         IX = MAX0(3,IX)
         GOTO 30
      ENDIF
   30 CONTINUE
C
C Encode tower address
      IF (XTRL(2) .GE. 0.) THEN
       IY = IY + 15
      ELSE
       IY = 16 - IY
      ENDIF
      IADDR    = IX + 16*(IY-1) + 512*(MODU-1)
C
  999 CONTINUE
      END
#endif
