*DK ehgeob
      SUBROUTINE EHGEOB (ILAY,X,Y,Z,U,V,W,XSEP,YSEP,ZTB,DTB,MFLSI)
C.----------------------------------------------------------------
C R.CLIFFT        mod Rumpf
C! Edges geom for barrel mod.
C *** CALCULATE EDGES GEOMETRY FOR BARREL MODULES
C Calls EHBARI
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "agcons.h"
C
      DATA NCALL /0/
      IF (NCALL.EQ.0)                   THEN
        NCALL = 1
        DTB=0.
        ZTB=0.
        TPHB2=TAN(PIBY12)
        CALL EHBARI(XMAX1,XMAX2,YMAX,ZMAX1,ZMAX2)
      ENDIF
      MFLSI=0
C
C *** EDGE SEPERATION FOR CURRENT COORDS
C
      IWSI=SIGN(1.,W)
      IF(ILAY.EQ.1) THEN
        XMAX = XMAX1
        ZMAX = ZMAX1
      ELSE
        XMAX = XMAX2
        ZMAX = ZMAX2
      ENDIF
      XWH=XMAX-(ZMAX-Z)*TPHB2
      YSEPC=YMAX- ABS(Y)
      XSEPC=XWH-ABS(X)
C
C *** EDGE SEPERATION AT BACK OR FRONT OF MODULE
C
      ZTB=ZMAX-IWSI*Z
      ZB=IWSI*ZMAX
      IF (ABS(W) .GT. 1.) W = SIGN(1.,W)
      TH=ACOS(W)
      PH=ATAN2(U,V)
      TANTH = TAN(TH)
      XB=X+ZTB*TANTH*SIN(PH)
      YB=Y+ZTB*TANTH*COS(PH)
      XSEPB=XMAX+ZMAX*TPHB2*(IWSI-1)-ABS(XB)
      YSEPB=YMAX-ABS(YB)
C
      IF(XSEPB.LE.0..OR.YSEPB.LE.0..OR.XSEPC.LE.0..OR.YSEPC.LE.0.)
     &   RETURN
      YSEP=0.5*(YSEPC+YSEPB)
      XSEP=0.5*(XSEPC+XSEPB)
C
C *** CALCULATE PATH LENGTH IN MODULE
C
      DTB=SQRT((X-XB)**2+(Y-YB)**2+(Z-ZB)**2)
      MFLSI=1
C
 50   CONTINUE
      RETURN
      END
#endif
