      SUBROUTINE DMIN2D(IFLAG,TPAR,IP,J1,JDIST,DMIN,SDIST,PERP)
CKEY   QIPBTAG / INTERNAL
C ----------------------------------------------------------------------
C! Finds the point of closest approach between a track and the IP in 2 d
C  Author   Dave Brown 10-12-93
C
C  INPUTS
C     IFLAG  :  1 (2) means r-phi (r-z) view
C     TPAR  :  Standard 5-parameter track description
C     IP    :  Interaction point
C     J1   :  Unit vector in the associated jet direction
C
C  OUTPUTS
C     JDIST :  Distance between IP and closest approach between the
C              track and the jet along jet direction
C     DMIN  :  Distance between track and IP at closest approach
C     SDIST :  Distance along track at closest approach to the IP
C     PERP  :  Direction between track and IP at closest approach
C
C  Called from FINDDMIN or MAKE2D
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER IFLAG,IFRFT
      REAL TPAR(5),TERR(4,4),DMIN,S_DMIN,PERP(3)
      REAL J1(3),JDIST,SDIST
C
C  Local variables
C
      INTEGER NUSED(2),ICOR
      REAL IP(3),SIG2_IP(3,3)
      REAL IR,TANL,PHI0,D0,Z0
      REAL COSL,SINL,COSP0,SINP0
      REAL T1(3),T0(3),S_DMIN2
      REAL LHAT(3),JHAT(3),NHAT(3),DENOM
      REAL*8 DJDIST,DSDIST,DDMIN,JDOT,LDOT,TDOT
C ----------------------------------------------------------------------
C
C  Unpack the track parameters
C
      IR  = TPAR(1)
      TANL= TPAR(2)
      PHI0 = TPAR(3)
      D0  = TPAR(4)
      Z0  = TPAR(5)
C
C  Calculate a few things
C
      COSL = 1./SQRT(1.+TANL**2)
      SINL = TANL*COSL
      COSP0 = COS(PHI0)
      SINP0 = SIN(PHI0)
C
C  Take a linear approximation to the tracks
C
      T1(1) =  COSL*COSP0
      T1(2) =  COSL*SINP0
      T1(3) =  SINL
      T0(1) =  D0*SINP0
      T0(2) =  -D0*COSP0
      T0(3) =  Z0
C
C  Depending on the view being projected, find the correct
C  projection directions.  PERP and LHAT form a basis in the
C  plane in which we have information, LHAT is along the track
C  direction and hence having impact parameter sign information,
C  PERP is perpendicular to the track direction and hence has impact
C  parameter size informaton.
C
      DENOM = SQRT(T1(1)**2 + T1(2)**2)
      IF(IFLAG.EQ.1)THEN
C
C  r-phi view
C
        PERP(1) =  T1(2)/DENOM
        PERP(2) = -T1(1)/DENOM
        PERP(3) =  0.0
        LHAT(1) =  T1(1)/DENOM
        LHAT(2) =  T1(2)/DENOM
        LHAT(3) =  0.0
      ELSE
C
C  r-z view
C
        PERP(1) = -T1(1)*T1(3)/DENOM
        PERP(2) = -T1(2)*T1(3)/DENOM
        PERP(3) =  DENOM
        LHAT(1) =  T1(1)
        LHAT(2) =  T1(2)
        LHAT(3) =  T1(3)
      END IF
C
C  Calculate the point of closest approach, and the distance between
C  the crossing point and either the IP or the track T0 point
C
      DJDIST = 0.0
      DSDIST = 0.0
      DDMIN = 0.0
      JDOT = 0.0
      LDOT = 0.0
      TDOT = 0.0
      DO ICOR=1,3
        JDOT = JDOT + J1(ICOR)*PERP(ICOR)
        LDOT = LDOT + J1(ICOR)*LHAT(ICOR)
        TDOT = TDOT + T1(ICOR)*LHAT(ICOR)
        DDMIN = DDMIN + (IP(ICOR)-T0(ICOR))*PERP(ICOR)
        DSDIST = DSDIST + (IP(ICOR)-T0(ICOR))*LHAT(ICOR)
      END DO
      IF(JDOT.NE.0.0)THEN
        DJDIST = -DDMIN/JDOT
      ELSE
        DJDIST = SIGN(1.0D-8,DDMIN)
      END IF
      DSDIST = (DSDIST+DJDIST*LDOT)/TDOT
C
C  Convert to single precision output variables
C
      JDIST = DJDIST
      SDIST = DSDIST
C
C  Sign DMIN according to which hemisphere the track-jet crossing
C  is in, relative to the IP.
C
      DMIN = SIGN(DDMIN,DJDIST)
C
C  Done
C
 1000 CONTINUE
      RETURN
      END
