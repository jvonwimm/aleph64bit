      SUBROUTINE TMOVE(D,TPAR,TERR,TARC,TRKPT,T1,AMAT)
CKEY   QIPBTAG / INTERNAL
C ------------------------------------------------------------------------------
C! Re-define the track parameters relative to a new point in space.
C  This also finds the momentum vector at that point, and diagonalizes
C  the error D0,Z0 part of the matrix at that point.
C  Called from CHIV0
C  Author                                             Dave Brown 4-9-93
C ------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  Inputs; point in space, track parameters and errors
C
      REAL D(3),TPAR(5),TERR(15)
C
C  Outputs;
C    TARC       : Transverse arc length
C    TRKPT      : Point on track nearest to space point,
C                 RELATIVE to D
C    T1         : Track direction unit vector at this point
C    AMAT       : Error weighted transverse distance matrix
C
      REAL TARC,TRKPT(3),T1(3),AMAT(3,3)
C
C  Local variables
C
      INTEGER ICOR,JCOR
      REAL RADIUS
      REAL TANL,PHI0,D0,Z0,COSP0,SINP0,PHI,COSP,SINP,COSL,SINL
      REAL THAT(3),PHAT(3)
      REAL NORM
      REAL PERR2(2)
C ------------------------------------------------------------------------------
C  Put track parameters into local variables
C
      RADIUS  = 1./TPAR(1)
      TANL= TPAR(2)
      PHI0 = TPAR(3)
      D0  = TPAR(4)
      Z0  = TPAR(5)
      COSP0 = COS(PHI0)
      SINP0 = SIN(PHI0)
      COSL = 1./SQRT(1.+TANL**2)
      SINL = TANL*COSL
C
C  Get the transverse arc length
C
      TARC = RADIUS*ATAN( (D(1)*COSP0+D(2)*SINP0)/
     &      (RADIUS - D0 + D(1)*SINP0 - D(2)*COSP0) )
C
C  Get position at this point
C
      PHI = PHI0+TARC/RADIUS
      COSP = COS(PHI)
      SINP = SIN(PHI)
      TRKPT(1) =  RADIUS*SINP-(RADIUS-D0)*SINP0 - D(1)
      TRKPT(2) = -RADIUS*COSP+(RADIUS-D0)*COSP0 - D(2)
      TRKPT(3) = Z0 + TANL*TARC                 - D(3)
C
C  Direction vector at this point
C
      T1(1) = COSP*COSL
      T1(2) = SINP*COSL
      T1(3) = SINL
C
C  Phi and theta directions
C
      NORM = SQRT(T1(1)**2+T1(2)**2)
      PHAT(1) = -T1(2)/NORM
      PHAT(2) =  T1(1)/NORM
      PHAT(3) = 0.0
      THAT(1) = -T1(1)*T1(3)/NORM
      THAT(2) = -T1(2)*T1(3)/NORM
      THAT(3) =  NORM
C
C  Correct the error matrix for the projective effect
C
      PERR2(1) = TERR(10)
      PERR2(2) = TERR(15)*COSL**2
C
C  Build the transverse distance matrix
C
      DO ICOR=1,3
        DO JCOR=1,3
          AMAT(JCOR,ICOR) = PHAT(ICOR)*PHAT(JCOR)/PERR2(1) +
     &          THAT(ICOR)*THAT(JCOR)/PERR2(2)
        END DO
      END DO
C
C  Done
C
      RETURN
      END
#endif
