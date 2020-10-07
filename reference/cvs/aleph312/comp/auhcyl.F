      SUBROUTINE AUHCYL( RAD, ZMAX, FIELD,Q, PAR, PAREX, ICODE)
C--------------------------------------------------------------------
C  M. MERMIKIDES   171186
C   Modified  9/5/87  Generalised for -ve sign mag field
C   Modified 22/7/96  Double precision on all platforms (M.Cattaneo)
C
C! Computes intersection of helix with a cylinder with axis along the
C  z-axis. We keep the first intersection with either the barrel or
C  endplane.
C
C  NB. The starting point of the helix should be within the cylinder
C      defined by RAD and ZMAX.
C
C  Input:
C        RAD   Radius of cylinder       (cm)
C        ZMAX  Half-length of cylinder. (cm)
C        FIELD Magnetic field in kG (assumed uniform, solenoidal)
C        Q     Charge of track (+- 1.)
C        PAR(1) = x
C           (2) = y      Coordinates of start point on the track (cm)
C           (3) = z
C           (4) = dx/ds
C           (5) = dy/ds  Direction cosines at start point
C           (6) = dz/ds
C           (7) = |p|    Abs. momentum of track (GeV/c)
C
C
C  Output:
C        PAREX(1) = x
C             (2) = y      Coordinates at point of intersection
C             (3) = z      with cylinder.
C             (4) = dxds
C             (5) = dxds   Dir. cosines at intersection point
C             (6) = dzds
C
C        ICODE  = Return code (1 = intersection in barrel part
C                              2     "          "   end cap
C                              0     "          not possible
C --------------------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
C
      DOUBLE PRECISION RHO,XC,YC,PHIC,CPH,SPH,UC,U0,CHORD,U,V
      DOUBLE PRECISION PX,PY,PZ,PT,U1,V1,VALU,BQ,DIR
C
      DIMENSION PAR(7),PAREX(6)
C
C  First check that the track starts from inside the cylinder
C
      ICODE = 0
      RSTA = SQRT(PAR(1)**2 + PAR(2)**2)
      IF (RSTA.GT.RAD.OR.ABS(PAR(3)).GT.ZMAX) GO TO 999
C
      PX = PAR(7)*PAR(4)
      PY = PAR(7)*PAR(5)
      PT = SQRT(PX**2 + PY**2)
      PZ = PAR(7)*PAR(6)
C
      BQ = Q
      IF(FIELD.LT.0.) BQ = -Q
      RHO = PT/(ABS(FIELD)*CLGHT*1.E-5)
      XC = BQ*RHO*PY/PT + PAR(1)
      YC =-BQ*RHO*PX/PT + PAR(2)
C
C  We work in a rotated system (u,v) such that the centre of
C  the circle lies on the +ve u-axis (UC).  U0 is the point of
C  closest approach to the origin in the u-v plane.  In this frame
C  positive tracks point upwards (+ve v) at U0.
C  PSIP is the turning angle from U0 to starting point of the track,
C  measured in a clockwise sense.
C
      PHIC = ATAN2(YC,XC)
      CPH = COS(PHIC)
      SPH = SIN(PHIC)
      UC = XC*CPH + YC*SPH
      U0 = UC-RHO
C
C  Find Z at the point of closest approach.
C  Take care of the case where the track starts upstream of d0 (U0)
C  (We assume track does not turn through more than pi)
C
      U1 = PAR(1)*CPH + PAR(2)*SPH
      V1 = PAR(2)*CPH - PAR(1)*SPH
      IF (BQ*V1.GT.0.) THEN
         DIR = 1.0
      ELSE
         DIR = -1.0
      ENDIF
      CHORD = SQRT((U1 - U0)**2 + V1*V1)
      VALU = CHORD/(2.*RHO)
      IF (VALU.GT.1.0) VALU = 1.0
      PSIP = 2.*ASIN(VALU)
      TANL = PZ/PT
      Z0 = PAR(3) - DIR*RHO*PSIP*TANL
      DMAX = UC + RHO
C
      IF (ABS(DMAX).LE.RAD) THEN
C
C  Intersection with cylinder is not possible. See if track intersects
C  endplane.
C
         Z = SIGN(ZMAX,PAR(6))
         SS = (Z-Z0)/TANL
C  PSI is turning angle from U0 to intersection; don't let it exceed 2pi
         PSI = SS/RHO
         IF (PSI.GE.TWOPI) GO TO 999
         U = UC + RHO*COS(PI-PSI)
         V = BQ*RHO*SIN(PI-PSI)
         PSI = PSI - PSIP
         ICODE = 2
C
C  Find intersection with cylinder
C
      ELSE
         U = (RAD*RAD - (RHO + UC)*(RHO - UC))/(2.*UC)
         V = BQ*SQRT( (RAD - U)*(RAD + U) )
         CHORD = SQRT((U1 - U)**2 + (V1 - V)**2)
         VALU = CHORD/(2.*RHO)
         IF (VALU.GT.1.0) VALU = 1.0
C  PSI is turning angle from (u,v) to intersection
         PSI = 2.*ASIN(VALU)
         Z = PAR(3) + RHO*PSI*TANL
         ICODE = 1
C
C  Cylinder crossed beyond ZMAX, compute intersection with endplane
C
         IF (ABS(Z).GT.ZMAX) THEN
            Z = SIGN(ZMAX,PAR(6))
            SS = (Z- Z0)/TANL
C  PSI is turning angle from d0 to intersection
            PSI = SS/RHO
            U = UC - RHO*COS(PSI)
            V =  BQ*RHO*SIN(PSI)
            PSI = PSI - PSIP
            ICODE = 2
         ENDIF
C
      ENDIF
C
      PAREX(1) = U*CPH - V*SPH
      PAREX(2) = U*SPH + V*CPH
      PAREX(3) = Z
      CP = COS(BQ*PSI)
      SP = SIN(BQ*PSI)
      PAREX(4) = (PAR(4)*CP + PAR(5)*SP)
      PAREX(5) = (PAR(5)*CP - PAR(4)*SP)
      PAREX(6) = PAR(6)
C
  999 RETURN
      END
#endif
