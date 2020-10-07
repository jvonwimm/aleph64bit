      SUBROUTINE LCLUTW(THETA,PHI,IADDR,DIST,ENE4,ENE9)
C-----------------------------------------------------------------------
C! Find central towers in a cluster
C!
C!   Author   : P. H. Hansen   890101
C!   Input    : THETA, PHI    barycenter direction (raw)
C!   Output   : IADDR         barycenter tower address
C!              DIST(2)       Hor/Ver distance from inner edge (signed)
C!              ENE4(3)       Storey energies of the four central towers
C!              ENE9(3)       Storey energies of the nine central towers
C!
C!   Description :
C!   ===========
C?      Extrapolate to reference plane (average shower max)
C?      Find distance to Lcal acceptance
C?      Find the corresponding tower address
C?      Find the surrounding towers
C?      Calculate barycenter from 3x3 towers and correct theta and phi
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "lcnamc.h"
#include "lcrejj.h"
#include "lmtyjj.h"
#include "lwrgjj.h"
#include "lypars.h"
#include "llayjj.h"
#include "alcons.h"
      DIMENSION DIST(*),ENE4(*),ENE9(*)
#include "bmacro.h"
C----------------------------------------------------------------------
      IADDR = 0
C Geometry banks
      KLMTY = IW(NALMTY)
      KLWRG = IW(NALWRG)
      KLLAY = IW(NALLAY)
      KLCRE = IW(NALCRE)
C
C Directions
      DIRX = SIN(THETA)*COS(PHI)
      DIRY = SIN(THETA)*SIN(PHI)
      DIRZ = COS(THETA)
      RADI = RTABL(KLCRE,1,JLCRZC)/ABS(COS(THETA))
C
C Module number
      MODU = 1
      IF(DIRX.GT.0.) MODU = 2
      IF(DIRZ.GT.0.) MODU = MODU + 2
C
C The wire support covers in each end
      DY1 = RTABL(KLMTY,1,JLMTDT)
C
C Impact point in first quadrant of reference plane
      X = ABS(DIRX*RADI)
      Y = ABS(DIRY*RADI)
      Z = RTABL(KLCRE,1,JLCRZC)
C
C   Find distance from inter-model crack
      DIST(1) = X - RTABL(KLWRG,1,JLWRXL)
      DIST(2) = 24.5
C
C   Find distance to "jagged" boundary
C   - approximate it by straight line:
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
C Column and row
      IY = MIN0(15,INT(Y/SIZE)+1)
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
      IF (DIRY .GE. 0.) THEN
        IY = IY + 15
      ELSE
        IY = 16 - IY
      ENDIF
      IADDR    = IX + 16*(IY-1) + 512*(MODU-1)
C
C Impact in pad coordinates
      XPAD = X/SIZE-FLOAT(IX)
      YPAD = SIGN(1.,DIRY)*Y/SIZE-FLOAT(IY)+15.5
C
C Find closest towers and new barycenter based on 3x3 towers
      CALL LFOURC(XPAD,YPAD,IADDR,ENE4,ENE9)
C
C Now correct the theta and phi according to the 3x3 estimate
      X = SIZE*(FLOAT(IX) + XPAD)
      Y = SIZE*(FLOAT(IY)-15.5 + YPAD)
      IF(DIRX.LT.0.) X = -X
      PHI = ATAN2(Y,X)
      IF(PHI.LT.0.) PHI=PHI+TWOPI
      THETA = ATAN(SQRT(X**2+Y**2)/Z)
      IF(DIRZ.LT.0.) THETA = PI - THETA
C
C Correct also the distance to the edge
      X = ABS(X)
      Y = ABS(Y)
      DIST(1) = X - RTABL(KLWRG,1,JLWRXL)
      DIST(2) = 24.5
      IF(Y.GT.RTABL(KLWRG,5,JLWRYL)+DY1) THEN
         DIS = X - (YEDGLC-Y)/TEDGLC
         IF(DIS.LT.DIST(1)) DIST(1)=DIS
         DIST(2) = Y - (YEDGLC-X*TEDGLC)
      ELSE
         DIST(1) = X - RTABL(KLWRG,6,JLWRXL)
      ENDIF
C
  999 CONTINUE
      END
#endif
