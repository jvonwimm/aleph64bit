      SUBROUTINE THTRAN(P,ORIG,DPHI,PA)
C
C----------------------------------------------------------------------
C! Transform helix to another coordinate system with z axis parallel
C! to the original
CKEY TPCDES HELIX TRANSFORM / USER
C   Author: R. Johnson   28-12-90
C Modified: M.Cattaneo   980310 Use DOUBLE PRECISION for SP0,CP0, to avoid 
C                               numerical problems
C
C     Input:
C       - P(5)       /R     Helix parameters in original frame
C                           1/r,tanl,phi0,d0,z0
C                           (d0>0 = positive ang. mom. about z axis)
C                           (r>0  = counterclockwise rotation)
C       - ORIG(3)    /R     Origin of the new coordinate system
C                           in the old system
C       - DPHI       /R     Phi of the x axis of the new coordinate
C                           system in the old system
C     Output:
C       - PA(5)     /R      Helix parameters in new frame.
C
C
C   Remarks:  only translations and rotations about the z axis are
C             possible in this case.  Rotations about x and y are
C             ignored since the helix model always assumes a helix
C             about the z axis.
C
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
#include "bcs.h"
C
      DIMENSION P(*),PA(*),ORIG(*)
      DOUBLE PRECISION XC,YC
      DOUBLE PRECISION SP0,CP0
      DATA EPS/0.001/
C
#include "bmacro.h"
C
      DX=ORIG(1)
      DY=ORIG(2)
      DZ=ORIG(3)
      IF (DX.EQ.0. .AND. DY.EQ.0.
     &        .AND. DZ.EQ.0. .AND. DPHI.EQ.0.) THEN
        DO 11 I=1,5
          PA(I)=P(I)
   11   CONTINUE
      ELSE
        SP0=SIN(P(3))
        CP0=COS(P(3))
C
C++     Translate the point of closest approach to old z axis to
C++     a point in the new coordinate system
C
        X=P(4)*SP0 - DX
        Y=-P(4)*CP0 - DY
        Z= P(5) - DZ
C
C++     Get the radius of curvature
C
        IF (P(1).EQ.0.) THEN
          RHO=1.0E22*SGN
          SGN=1.0
          CALL ALTELL ('THTRAN: Inverse radius of curvature=0.',
     &                  0,'RETURN')
        ELSE
          RHO=1.0/P(1)
          SGN=SIGN(1.0,P(1))
        ENDIF
C
C++     Find the point of closest approach to the new z axis.
C++     Get D0.  For high momentum tracks, expand the sqare root
C++     in order to avoid numerical problems.
C
        B=(X**2 + Y**2)*(P(1)**2)
        A=2.0*P(1)*(CP0*Y - SP0*X)
        IF (ABS(A).LT.EPS) THEN
          D0=-RHO*0.5*(A+B)
        ELSEIF (ABS(B).LT.EPS) THEN
          RADC=SQRT(1.0+A)
          D0=RHO*(1.0-RADC-0.5*B/RADC)
        ELSE
          D0=RHO*(1.0-SQRT(1.0+A+B))
        ENDIF
C
C++     Find the center of the circle and get phi0
C
        XC=-DBLE(RHO*SP0)+DBLE(X)
        YC= DBLE(RHO*CP0)+DBLE(Y)
        PHIC=DATAN2(YC,XC)
        PHI0=PHIC-SGN*PIBY2
        IF (PHI0.LT.0.) THEN
          PHI0=PHI0+TWOPI
        ELSEIF (PHI0.GE.TWOPI) THEN
          PHI0=PHI0-TWOPI
        ENDIF
C
C++     Find the turning angle from the old point to the new
C
        ALPHA=P(3)-PHI0
        IF (ALPHA.LT.-PI) THEN
          ALPHA=ALPHA+TWOPI
        ELSEIF (ALPHA.GE.PI) THEN
          ALPHA=ALPHA-TWOPI
        ENDIF
C
C++     Get the new Z at the point of closest approach
C
        Z0= Z - RHO*ALPHA*P(2)
C
C++     Finally rotate around new z axis and output results
C
        PA(1)=P(1)
        PA(2)=P(2)
        PA(3)=PHI0-DPHI
        PA(4)=D0
        PA(5)=Z0
      ENDIF
C
      END
#endif
