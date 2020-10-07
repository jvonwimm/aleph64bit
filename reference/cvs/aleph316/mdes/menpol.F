      SUBROUTINE MENPOL(NSLOT,ISUBC,LAYER,XX,YY,RHO,THETA,PHI)
C------------------------------------------------------------------
C! muon endcaps : convert local XX,YY to polar coordinates
C!
C!   F.Bossi/G.Capon/D.Kuhn             861107
C!
C------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "mupar1.h"
#include "mcg1da.h"
#include "mrddat.h"
#include "alcons.h"
C
C
      NSL=NSLOT-NSLBAR-NSLMDA
      NSL1=1+MOD(NSL-1,4)
      IF(NSL.LE.8)THEN
C
C?      A side
C
         SIGNZ=1.
         IF(NSL1.EQ.1.OR.NSL1.EQ.4)THEN
            XEC=YY
         ELSE
            XEC=-YY
         END IF
C
C?      B side
C
      ELSE
         SIGNZ=-1.
         IF(NSL1.LE.2)THEN
            XEC=YY
         ELSE
            XEC=-YY
         END IF
      ENDIF
C
C?     position of modules in individual slots taken into account
C?     XEC and YEC are now hit coordinates in aleph-system
C
      IF(NSL1.EQ.1)YEC=SIGNZ*(YWIDTH(NSL)-XX)
      IF(NSL1.EQ.2)YEC=XX
      IF(NSL1.EQ.3)YEC=SIGNZ*(-YWIDTH(NSL)+XX)
      IF(NSL1.EQ.4)YEC=-XX
      DX=ABS(XCENTR(NSL))-XWIDTH(NSL)/2.
      DY=ABS(YCENTR(NSL))-YWIDTH(NSL)/2.
      XEC=XEC+DX*(XCENTR(NSL)/ABS(XCENTR(NSL)))*SIGNZ
      YEC=YEC+DY*(YCENTR(NSL)/ABS(YCENTR(NSL)))
      PHI=ATAN2(YEC,XEC)
      IF(PHI.LT.0.)PHI=PHI+TWOPI
      RR=SQRT(XEC*XEC+YEC*YEC)
      ZENC=SIGNZ*(ZENCAP(NSL)+0.5*THICKC+ZZOFFS(ISUBC,LAYER))
      RHO = SQRT(RR*RR + ZENC*ZENC)
      THETA=ACOS(ZENC/RHO)
C
      RETURN
      END
#endif
