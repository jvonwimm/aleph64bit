      SUBROUTINE SRALIG
C----------------------------------------------------------------------
C! Perform SATR alignment correction
C!
C!    Author:     H. Meinhard       06-Apr-1988
C!    Modified:   H. Meinhard       07-Oct-1989  (2)
C!
C!    Description
C!    ===========
C!    For each SATR track and each SATR patch parameter, get theta and
C!    phi and correct according to the alignment constants. Store new
C!    parameters.
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "saligc.h"
#include "sanamc.h"
#include "sgeomc.h"
#include "strkjj.h"
#include "bosext.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C mean value of phi start angles of layers
      PHIB = (PHIBSG(1)+PHIBSG(2)+PHIBSG(3))/3.
C
C link to track bank
        KSTRK = IW(NASTRK)
        IF (KSTRK .EQ. 0)                                   GOTO 999
C
C top of loop over tracks/patches
        DO 300 ITRK = 1, LROWS(KSTRK)
          TTHET = TAN(RTABL(KSTRK,ITRK,JSTRTH))
          PHI = RTABL(KSTRK,ITRK,JSTRPH)
C
C get module number IMOD
          IMOD = 1
          IF (MOD(PHI+TWOPI-PHIB,TWOPI) .GT. PI) IMOD = 2
          IF (TTHET .LT. 0.) IMOD = IMOD+2
C
C compute impact point of track through fifth module layer
          ZPRIM =  -ZOFFSG - FLOAT(MLAYSG-1)*ZDELSG/2.
          Z = ZZERSG + ZPRIM
          IF (IMOD .GE. 3) THEN
            ZPRIM = -ZPRIM
            Z = -Z
          ENDIF
          XPRIM = Z*TTHET*COS(PHI)
          YPRIM = Z*TTHET*SIN(PHI)
C
C alignment correction
          X = XPRIM + XSHISA(IMOD) - XYANSA(IMOD)*YPRIM +
     +          ZXANSA(IMOD)*ZPRIM
          Y = YPRIM + YSHISA(IMOD) - YZANSA(IMOD)*ZPRIM +
     +          XYANSA(IMOD)*XPRIM
          Z = ZPRIM + ZSHISA(IMOD) - ZXANSA(IMOD)*XPRIM +
     +          YZANSA(IMOD)*YPRIM
C
C get new theta and phi, store it
          THETA = ATAN(SQRT(X**2+Y**2)/Z)
          IF (THETA .LT. 0.) THETA = THETA+PI
          PHI = ATAN2(Y,X)
          IF (PHI .LT. 0) PHI = PHI+TWOPI
          RW(KROW(KSTRK,ITRK)+JSTRTH) = THETA
          RW(KROW(KSTRK,ITRK)+JSTRPH) = PHI
C
C bottom of loop over tracks/patches
  300   CONTINUE
C
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
#endif
