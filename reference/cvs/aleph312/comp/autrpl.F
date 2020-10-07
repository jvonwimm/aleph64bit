      SUBROUTINE AUTRPL(XPL,PHI,HWID,FIELD,Q,PAR,PAREX,ICODE)
C-------------------------------------------------------------------
C  M. MERMIKIDES 10/12/86
C!Compute intersection of track with plane parallel to the z-axis.
C Plane has width +-HWID and infinite extent in Z
C
C   Input:  XPL   =  Normal distance of plane from origin
C           PHI   =  Orientation of normal to plane wrt x-axis.
C           HWID  =  Half width of plane (along local y)
C           FIELD =  Magnetic field
C           Q     =  Sign of curvature (+ve = clockwise)
C           PAR(1)=  x
C               2 =  y    Coordinates of starting point on helix
C               3 =  z
C               4 =  dx/ds
C               5 =  dy/ds  Direction cosines at point (x,y,z)
C               6 =  dx/ds
C               7 =  Abs. value of momentum
C   Output:
C         PAREX(1)=  x
C               2 =  y    Coordinates at intersection with plane
C               3 =  z
C               4 =  dx/ds
C               5 =  dy/ds  Direction cosines at point (x,y,z)
C               6 =  dx/ds
C           ICODE = 0  If intersection is not possible
C                 = 1  One intersection found over width of plane
C                 = 2  Two intersections found over width of plane
C                      (we keep the one closest to starting point)
C                 = 3  If intersection is outside the width
C                      (Only for neutral track case for now)
C -------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
      DIMENSION PAR(7),PAREX(6)
C
      ICODE = 0
      CP = COS(PHI)
      SP = SIN(PHI)
C
C  Neutral, or no field.  Compute intersection of line with plane
C
      IF (NINT(Q).EQ.0.OR.NINT(FIELD).EQ.0) THEN
C
C  Intersection in x-y plane
C     track represented by Y = A1 + B1*X (Rotated to ref sys of plane)
C     plane represented by X = XPL
C
         X1 = PAR(1)*CP + PAR(2)*SP
         Y1 = PAR(2)*CP - PAR(1)*SP
         DXDS = PAR(4)*CP + PAR(5)*SP
         DYDS = PAR(5)*CP - PAR(4)*SP
C  Compute intersection only if track is directed towards the plane
         IF(DXDS.LT.0.) GO TO 999
C  Skip if track is parallel to plane
         IF(ABS(DXDS).LT.1.E-4) GO TO 999
         B1 = DYDS/DXDS
         A1 = Y1 - X1*B1
         X2 = XPL
         Y2 = A1 + B1*XPL
         Z2 = PAR(3) + PAR(6)*(X2-X1)/DXDS
         ICODE = 1
         IF(ABS(Y2).GT.HWID) ICODE = 3
C
         PAREX(1) = X2*CP - Y2*SP
         PAREX(2) = Y2*CP + X2*SP
         PAREX(3) = Z2
         PAREX(4) = PAR(4)
         PAREX(5) = PAR(5)
         PAREX(6) = PAR(6)
C
      ELSE
C
C  Charged track. Find intersection of helix with plane
C
         PX = PAR(7)*PAR(4)
         PY = PAR(7)*PAR(5)
         PT = SQRT(PX**2 + PY**2)
         PZ = PAR(7)*PAR(6)
         RHO = PT/(FIELD*CLGHT*1.E-5)
         XCEN = Q*RHO*PY/PT + PAR(1)
         YCEN =-Q*RHO*PX/PT + PAR(2)
C  Transform startpoint and track curvature centre to ref sys of plane
C  (rotation by  -phi about z-axis)
         X1 = PAR(1)*CP + PAR(2)*SP
         Y1 = PAR(2)*CP - PAR(1)*SP
         Z1 = PAR(3)
         XC = XCEN*CP + YCEN*SP
         YC = YCEN*CP - XCEN*SP
         X2 = XPL
         TERM = RHO**2 - (X2 - XC)**2
         IF(TERM.LT.0.) GO TO 999
C  Intersections of track at plane at x = X2
         YS1 = YC + SQRT(TERM)
         YS2 = YC - SQRT(TERM)
         ISOL = 0
         IF (ABS(YS1).LE.HWID) THEN
            AS1 = SQRT((X2-X1)**2 + (YS1-Y1)**2)/(2.0*RHO)
            IF (AS1.LE.1.0) ISOL = ISOL + 1
         ENDIF
         IF (ABS(YS2).LE.HWID) THEN
            AS2 = SQRT((X2-X1)**2 + (YS2-Y1)**2)/(2.0*RHO)
            IF (AS2.LE.1.0) ISOL = ISOL + 2
         ENDIF
         IF (ISOL.EQ.0) THEN
            GO TO 999
         ELSEIF (ISOL.EQ.1) THEN
            Y2 = YS1
            AS = AS1
            ICODE = 1
         ELSEIF (ISOL.EQ.2) THEN
            Y2 = YS2
            AS = AS2
            ICODE = 1
         ELSE
C
C If we have two intersections keep the one nearest to the start
C point.
C
            IF (AS1.LT.AS2) THEN
               Y2 = YS1
               AS = AS1
            ELSE
               Y2 = YS2
               AS = AS2
            ENDIF
            ICODE = 2
         ENDIF
C
         PSI = 2.*ASIN(AS)
         Z2 = Z1 + RHO*PSI*PZ/PT
C
C  Now rotate intersection back to global reference frame and "swim"
C  direction cosines.
C
         PAREX(1) = X2*CP - Y2*SP
         PAREX(2) = Y2*CP + X2*SP
         PAREX(3) = Z2
         CPSI = COS(Q*PSI)
         SPSI = SIN(Q*PSI)
         PAREX(4) = PAR(4)*CPSI + PAR(5)*SPSI
         PAREX(5) = PAR(5)*CPSI - PAR(4)*SPSI
         PAREX(6) = PAR(6)
C
      ENDIF
C
  999 RETURN
      END
#endif
