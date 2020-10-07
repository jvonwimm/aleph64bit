      SUBROUTINE DMINERR(TPAR,TERR,PERP,JPERP,JETSIG2,JDIST,
     &     SDIST,SIG2_IP,S_DMIN,S_LDIST,PHIP)
CKEY   QIPBTAG / INTERNAL
C -----------------------------------------------------------------------------
C! Calculate the error on the impact parameter
C   Author  Dave Brown, 11-12-93
C
C  INPUTS
C        TPAR    :  Standard 5 track parameters
C        TERR    :  Track parameter errors in 4X4 form
C        T1      :  Unit vector in track direction
C        PERP    :  Unit vector in the impact parameter direction
C        JPERP   :  Unit vector in the track-jet closest approach direction
C        JETSIG2 :  Angular error on jet direction
C        JDIST   :  Distance along jet of track-jet closest approach
C        SDIST   :  Distance along track of track-IP closest approach
C        SIG2_IP :  Primary vertex error matrix
C
C  OUTPUTS
C        S_DMIN  :  Error on impact parameter
C        S_LDIST :  Error on track-jet approach distance
C        PHIP    :  Angle of impact parameter at jet closest approach
C
C  Called from FINDDMIN or MAKE2D
C -----------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  IO variables
C
      REAL TPAR(5),TERR(4,4),T1(3),PERP(3),JPERP(3)
      REAL JETSIG2,JDIST,SDIST
      REAL SIG2_IP(3,3)
      REAL S_DMIN,S_LDIST,PHIP
C
C  Local variables
C
      INTEGER ICOR,JCOR,IPAR,JPAR
      REAL THAT(3),PHAT(3),DENOM
      REAL DOTT,DOTP,JDOTT,JDOTP
      REAL DERV(4),JDERV(4)
      REAL IR,TANL,COSL,SINL,D0,PHI0,Z0
      REAL COSP0,SINP0
      REAL*8 SIG2,SIGL2,MINSIG2
      DATA MINSIG2/6.25D-6/
C -----------------------------------------------------------------------------
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
C  Unit vector in track direction
C
      T1(1) =  COSL*COSP0
      T1(2) =  COSL*SINP0
      T1(3) =  SINL
C
C  Compute the unit vectors for this track in the r-phi,r-z coordinate system
C
      DENOM = SQRT(T1(1)**2 + T1(2)**2)
      PHAT(1) =  T1(2)/DENOM
      PHAT(2) = -T1(1)/DENOM
      PHAT(3) =  0.0
      THAT(1) = -T1(1)*T1(3)/DENOM
      THAT(2) = -T1(2)*T1(3)/DENOM
      THAT(3) =  DENOM
C
C  Save the projection of PERP and JPERP onto these directions
C
      DOTT = 0.0
      DOTP = 0.0
      JDOTT = 0.0
      JDOTP = 0.0
      DO ICOR=1,3
        DOTT = DOTT + PERP(ICOR)*THAT(ICOR)
        DOTP = DOTP + PERP(ICOR)*PHAT(ICOR)
        JDOTT = JDOTT + JPERP(ICOR)*THAT(ICOR)
        JDOTP = JDOTP + JPERP(ICOR)*PHAT(ICOR)
      END DO
C
C  compute the angle at the jet closest approach
C
      IF(JDOTP.NE.0.0.AND.JDOTT.NE.0.0)THEN
        PHIP = ATAN2(JDOTP,JDOTT)
      ELSE
        PHIP = 0.0
      END IF
C
C  Compute the derrivatives of dmin for the 4 linear track parameters
C  (tanl,phi0,d0,z0, in that order).
C
      DERV(1) =  -SDIST*DOTT*COSL**2
      DERV(2) =  SDIST*COSL*DOTP+D0*SINL*DOTT
      DERV(3) =  -DOTP
      DERV(4) =  -COSL*DOTT
C
C  Ignore the track angle error terms for the jet distance; they
C  are small compared to the jet angle error
C
      JDERV(1) = 0.0
      JDERV(2) = 0.0
      JDERV(3) = JDOTP
      JDERV(4) = JPERP(3)
C
C  Now, compute the track errors.  The ldist error includes the
C  jet direction uncertainty, which couples to the observed crossing
C  distance.  This is actually only an approximation, the true effect
C  of the jet angle error is highly non-gaussian when the track and the
C  jet are nearly colinear.
C
      SIG2 = 0.0
      SIGL2 = JETSIG2*JDIST**2
      DO IPAR=1,4
        DO JPAR=1,4
          SIG2 = SIG2 + DERV(IPAR)*DERV(JPAR)*TERR(IPAR,JPAR)
          SIGL2 = SIGL2 + JDERV(IPAR)*JDERV(JPAR)*TERR(IPAR,JPAR)
        END DO
      END DO
C
C  Take a minimum track error to be what we see with muon pairs
C  (about 25 microns)
C
      SIG2 = MAX(SIG2,MINSIG2)
      SIGL2 = MAX(SIGL2,MINSIG2)
C
C  Add in the error coming from the IP.  Since the track has been
C  explicitly removed, there's no correlation.
C
      DO ICOR=1,3
        DO JCOR=1,3
          SIG2 = SIG2 + SIG2_IP(ICOR,JCOR)*PERP(ICOR)*PERP(JCOR)
          SIGL2 = SIGL2 + SIG2_IP(ICOR,JCOR)*JPERP(ICOR)*JPERP(JCOR)
        END DO
      END DO
C
C  Finish the error calculation, being careful with the sqrt.
C
      S_DMIN = SQRT(MAX(SIG2,MINSIG2))
      S_LDIST = SQRT(MAX(SIGL2,MINSIG2))
C
C  Done
C
      RETURN
      END
#endif
