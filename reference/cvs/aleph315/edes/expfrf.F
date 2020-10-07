      SUBROUTINE EXPFRF( RCYL, ZCYL, P, Q, ICODE)
C--------------------------------------------------------------------
CKEY ECALDES EXTRAPOLATE TRACK CYLINDER / USER
C     H.Videau      Creation 01/03/89
C
C     C. Mannert    16/03/98
C                   Correct treatment of d0 sign
C      
C! Extrapolate a PFRF type track to a cylinder.
C Similar to AUHCYL but takes as standard input a PFRF track.
C The cylinder has its axis along Z, its radius is Rcyl and it is
C limited to + - Zcyl. If the radius is larger than the TPC inner
C radius, the multiple scattering is taken into account.
C  If the radius is too small to reach the barrel, tries to get
C the end plates; if the TanLamb is too small it does just one turn.
C
C  Input:
C        RCYL     Radius of cylinder       (cm)
C        ZCYL     Half-length of cylinder. (cm)
C        P(1) = Inverse radius
C         (2) = Tangent Lambda
C         (3) = Phi0
C         (4) = D0
C         (5) = Z0
C         (6) = Alpha multiple scattering angle
C
C   Output:
C        Q(1) = x
C         (2) = y      Coordinates at point of intersection
C         (3) = z      with cylinder.
C         (4) = px/pt
C         (5) = py/pt    momentum at the intersection point
C         (6) = pz/pt
C
C        ICODE  = Return code (1 = intersection with cylinder
C                              2     "          "   flat caps
C                              0     "          not possible
C   Calls: UNEWDP from ALEPHLIB
C   Called by USER
C     
C--------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      SAVE
#include "alcons.h"
C  Input
      REAL RCYL,ZCYL,P(6)
C  Output
      REAL Q(6)
      INTEGER ICODE
C Locales
      INTEGER IINRD,ITLBD,IPHI0,IDIST,IZCOR,IALPH
      PARAMETER(IINRD=1,ITLBD=2,IPHI0=3,IDIST=4)
      PARAMETER(IZCOR=5,IALPH=6)
      REAL DIST,PHI0
      REAL SINPHI,COSPHI,XORG,YORG,ZORG
      REAL UC,XC,YC,D1,D2,DMIN,DMAX
      REAL X,Y,Z,SS,KSI,PSI,U,VALU
C Execution
      DIST=P(IDIST)
      PHI0=P(IPHI0)
      IF(RCYL.GT.31.) CALL UNEWDP(P,DIST,PHI0)
      SINPHI=SIN(PHI0)
      COSPHI=COS(PHI0)
C closest point to the axis
      XORG=DIST*SINPHI
      YORG=-DIST*COSPHI
      ZORG=P(IZCOR)
      
C centre of the helix
      UC=1./P(IINRD)
      XC = (-UC + DIST)*SINPHI
      YC =-(-UC + DIST)*COSPHI
      
C condition for helix crossing cylinder
      D1=ABS(DIST)
      D2=ABS(2.*UC-DIST)
      
      DMIN=AMIN1(D1,D2)
      DMAX=AMAX1(D1,D2)
      
      IF(RCYL.GT.DMAX.OR.RCYL.LT.DMIN) THEN
C  No intersection with cylinder
        IF (ABS(P(ITLBD)).GT.0.01) THEN
          Z = SIGN(ZCYL,P(ITLBD))
          SS = (Z-ZORG)/P(ITLBD)
          KSI=SS*P(IINRD)+PHI0
          ICODE = 2
        ELSE
          KSI=SIGN(TWOPI,P(IINRD))+PHI0
          Z=ZORG+P(ITLBD)*TWOPI*ABS(UC)
          ICODE=0
        ENDIF
C   PSI is turning angle from x axis to intersection;
        PSI = KSI+PIBY2
        X = XC - COS(PSI)*UC
        Y = YC - SIN(PSI)*UC
      ELSE
C intersection with cylinder
        VALU=((UC-DIST)**2+UC**2-RCYL**2)/(2.*(UC-DIST)*UC)
        IF(VALU.GT.1.)VALU=1.
        IF(VALU.LT.-1.)VALU=-1.
C  PSI is turning angle from (u,v) to intersection
        PSI=ACOS(VALU)*SIGN(1.,UC)
        Z = ZORG + PSI*P(ITLBD)*UC
        KSI=PSI+PHI0
        X=XC-UC*COS(KSI+PIBY2)
        Y=YC-UC*SIN(KSI+PIBY2)
        ICODE = 1
        
C
C  Cylinder crossed beyond ZCYL, compute intersection with endplane
C
        IF (ABS(Z).GT.ZCYL) THEN
          IF (ABS(P(ITLBD)).GT.0.01) THEN
            Z = SIGN(ZCYL,P(ITLBD))
            SS = (Z-ZORG)/P(ITLBD)
            KSI=SS*P(IINRD)+PHI0
            ICODE = 2
          ELSE
            Z=ZORG+P(ITLBD)*TWOPI*ABS(UC)
            KSI=SIGN(TWOPI,P(IINRD))+PHI0
            ICODE=0
          ENDIF
C       PSI is turning angle from x axis to intersection;
          PSI = KSI+PIBY2
          X = XC - COS(PSI)*UC
          Y = YC - SIN(PSI)*UC
        END IF
      END IF
C
      Q(1) = X
      Q(2) = Y
      Q(3) = Z
      Q(4) = COS(KSI)
      Q(5) = SIN(KSI)
      Q(6) = P(ITLBD)
C
  999 CONTINUE
      END
#endif
