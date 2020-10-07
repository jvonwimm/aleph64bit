      SUBROUTINE TNRHPA(P,X,Q,B,HELIX,XC,S)
C
C---------------------------------------------------------------------
C! Return helix parameters of a track element
C!
C!    Author:   R. Johnson    12-08-87
C!    Modified: R. Johnson    17-04-91  make all variables double prec
C!
C!    Input:
C!       - P(3)     /R      Particle 3-momentum at point X
C!       - X(3)     /R      Point at which P is given
C!       - Q        /R      Charge of the particle
C!       - B        /R      Magnitude of the magnetic field (in +z direc
C!    Output:
C!       - HELIX(*) /R      Helix parameters with respect to the
C!                          detector origin:
C!                           r,phi0,d0,z0,tanl
C!       - XC(2)    /R      Center of the helix circle
C!       - S        /R      Distance from closest approach to origin to
C!                          the point X, along the track.
C!
C!-------------------------------------------------------------------
#ifndef DOC
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      SAVE FIRST,TWOPI,PI,PIBY2,CLGHT
      REAL P(*),X(*),Q,B,HELIX(*),XC(*),S
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        PI=4.D0*DATAN(1.D0)
        TWOPI=2.D0*PI
        PIBY2=PI/2.D0
        CLGHT=29.9792458D0
      ENDIF
C
C++   Find the radius of curvature and the center of the circle
C
      PT = DSQRT(DBLE(P(1))**2 + DBLE(P(2))**2)
      RHO= -DBLE(Q)*PT/(DBLE(B)*CLGHT*1.0D-5)
      SGN= DSIGN(1.D0,RHO)
C...Special treatment of charged track with PT=0
      IF(PT.EQ.0) THEN
        XC(1)=DBLE(X(1))
        XC(2)=DBLE(X(2))
        IF (XC(1).EQ.0. .AND. XC(2).EQ.0.) THEN
          PHIC=0.
        ELSE
          PHIC = DATAN2(DBLE(XC(2)),DBLE(XC(1)))
        ENDIF
        PHI0 = PHIC - PIBY2
        IF (PHI0.LT.0.) THEN
          PHI0 = PHI0 + TWOPI
        ELSEIF (PHI0.GE.TWOPI) THEN
          PHI0 = PHI0 - TWOPI
        ENDIF
        HELIX(1)=0
        HELIX(2)=0
        HELIX(3)=PHI0
        HELIX(4)=DSQRT(DBLE(X(1))**2+DBLE(X(2))**2)
        HELIX(5)=0.
        S=DBLE(ABS(X(3)))
        GOTO 999
      ENDIF
C
C++   Signed distance of closest approach to the origin and the
C++   track direction in the plane at that point.
C
      XC8=-RHO*DBLE(P(2))/PT + DBLE(X(1))
      YC8= RHO*DBLE(P(1))/PT + DBLE(X(2))
      RC = DSQRT(XC8**2+YC8**2)
      D0 = RHO - SGN*RC
C
      XC(1)=XC8
      XC(2)=YC8
      IF (XC(1).EQ.0. .AND. XC(2).EQ.0.) THEN
        PHIC=0.
      ELSE
        PHIC = DATAN2(DBLE(XC(2)),DBLE(XC(1)))
      ENDIF
      PHI0 = PHIC - SGN*PIBY2
      IF (PHI0.LT.0.) THEN
        PHI0 = PHI0 + TWOPI
      ELSEIF (PHI0.GE.TWOPI) THEN
        PHI0 = PHI0 - TWOPI
      ENDIF
C
C++   Find the distance along the track from origin to X
C
      THETA=DATAN2(DBLE(P(2)),DBLE(P(1)))
      IF (THETA.LT.0.) THETA=THETA+TWOPI
      ALPHA=THETA-PHI0
      IF (ALPHA.LT.-PI) THEN
        ALPHA=ALPHA+TWOPI
      ELSEIF (ALPHA.GT.PI) THEN
        ALPHA=ALPHA-TWOPI
      ENDIF
      SP=RHO*ALPHA
C
C++   Find the dip angle and z position at approach to origin
C
      TANL = DBLE(P(3))/PT
      Z0 = X(3) - SP*TANL
C
      HELIX(1) = 1.D0/RHO
      HELIX(2) = TANL
      HELIX(3) = PHI0
      HELIX(4) = D0
      HELIX(5) = Z0
      S= SP*DSQRT(1.D0+TANL**2)
C
 999  CONTINUE
      RETURN
      END
#endif
