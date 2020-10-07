      SUBROUTINE V0TPAR(D,DERR,TMOM,RADIUS,TERR,V0PAR,V0ERR)
CKEY   QIPBTAG / INTERNAL
C ----------------------------------------------------------------------
C! Calculate the track parameters for a V0 candidate.
C  Called from QFNDV0
C  Author                                                D. Brown 9-9-93
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Inputs; The V0 decay point, error matrix of that point,
C  Tracks and their upper-corner error matrix.
C
      REAL D(3),DERR(3,3),P(3),TMOM(3,2),RADIUS(2)
      REAL TERR(3,3,2)
C
C  Outputs; 5 track parameters and error matrix
C
      REAL V0PAR(5),V0ERR(5,5)
C
C  Local variables
C
      INTEGER ITRK,ICOR,JCOR
      INTEGER IPAR,JPAR,KPAR,LPAR
      REAL PT,VP,V0MOM(3),VPT,SINP,COSP,TANL
      REAL PERR(3,3)
      REAL TDERR(3,3,2),PDERR(3,3),DDERR(3,4:5),UDERR(3,4:5)
C ----------------------------------------------------------------------
C  First, construct the momentum vector error matrix.  This requires
C  a few derivatives
C
      DO ITRK=1,2
        PT = SQRT(TMOM(1,ITRK)**2+TMOM(2,ITRK)**2)
C
C  Inverse radius derrivatives
C
        DO ICOR=1,3
          TDERR(ICOR,1,ITRK) = -TMOM(ICOR,ITRK)*RADIUS(ITRK)
        END DO
C
C  TANL derrivatives
C
        TDERR(1,2,ITRK) = 0.0
        TDERR(2,2,ITRK) = 0.0
        TDERR(3,2,ITRK) = PT
C
C  PHI0 derrivatives
C
        TDERR(1,3,ITRK) = -TMOM(2,ITRK)
        TDERR(2,3,ITRK) =  TMOM(1,ITRK)
        TDERR(3,3,ITRK) =  0.0
      END DO
C
C  Build the matrix
C
      DO ICOR=1,3
        DO JCOR=1,3
          PERR(ICOR,JCOR) = 0.0
          DO ITRK=1,2
            DO IPAR=1,3
              DO JPAR=1,3
                PERR(ICOR,JCOR) = PERR(ICOR,JCOR) +
     &                TERR(IPAR,JPAR,ITRK)*
     &              TDERR(ICOR,IPAR,ITRK)*TDERR(JCOR,JPAR,ITRK)
              END DO
            END DO
          END DO
        END DO
      END DO
C
C  From this, we can make the upper corner of the final matrix
C  First, build the parameters
C
      VP = 0.0
      DO ICOR=1,3
        V0MOM(ICOR) = TMOM(ICOR,1)+TMOM(ICOR,2)
        VP = VP + V0MOM(ICOR)**2
      END DO
      VPT = SQRT(V0MOM(1)**2+V0MOM(2)**2)
      VP = SQRT(VP)
      TANL = V0MOM(3)/VPT
      COSP = V0MOM(1)/VPT
      SINP = V0MOM(2)/VPT
      V0PAR(1) = VP
      V0PAR(2) = TANL
      V0PAR(3) = ATAN2(SINP,COSP)
C
C  Now derrivatives, in terms of the V0 momentum components
C  First, V0 momentum derrivatives
C
      DO ICOR=1,3
        PDERR(ICOR,1) = V0MOM(ICOR)/VP
      END DO
C
C  V0 TANL derrivatives
C
      PDERR(1,2) = -TANL*COSP/VPT
      PDERR(2,2) = -TANL*SINP/VPT
      PDERR(3,2) = 1./VPT
C
C  V0 phi0 derrivatives
C
      PDERR(1,3) = -SINP/VPT
      PDERR(2,3) =  COSP/VPT
      PDERR(3,3) = 0.0
C
C  Construct the upper corner of the matrix
C
      DO IPAR=1,3
        DO JPAR=1,3
          V0ERR(IPAR,JPAR) = 0.0
          DO ICOR=1,3
            DO JCOR=1,3
              V0ERR(IPAR,JPAR) = V0ERR(IPAR,JPAR) +
     &              PERR(ICOR,JCOR)*PDERR(ICOR,IPAR)*PDERR(JCOR,JPAR)
            END DO
          END DO
        END DO
      END DO
C
C  Now we bootstrap from these elements to get the lower corner
C  (impact parameters).  First, compute the values
C
      V0PAR(4) = D(1)*SINP-D(2)*COSP
      V0PAR(5) = D(3)-TANL*(D(1)*COSP+D(2)*SINP)
C
C  Now some derrivatives; this time in terms of the decay
C  point and the phi0,tanl terms already in the matrix.  First,
C  point derrivatives
C
C  D0
C
      DDERR(1,4) =  SINP
      DDERR(2,4) = -COSP
      DDERR(3,4) = 0.0
C
C  Z0
C
      DDERR(1,5) = -TANL*COSP
      DDERR(2,5) = -TANL*SINP
      DDERR(3,5) = 1.0
C
C  Now upper corner terms
C
      UDERR(1,4) = 0.0
      UDERR(2,4) = 0.0
      UDERR(3,4) = D(1)*COSP + D(2)*SINP
C
      UDERR(1,5) = 0.0
      UDERR(2,5) = -D(1)*COSP-D(2)*SINP
      UDERR(3,5) = TANL*(D(1)*SINP-D(2)*COSP)
C
C  Fill in the remaining parts of the matrix
C
      DO IPAR=4,5
        DO JPAR=4,5
          V0ERR(IPAR,JPAR) = 0.0
          DO ICOR=1,3
            DO JCOR=1,3
              V0ERR(IPAR,JPAR) = V0ERR(IPAR,JPAR) +
     &              DERR(ICOR,JCOR)*DDERR(ICOR,IPAR)*DDERR(JCOR,JPAR)
            END DO
          END DO
          DO KPAR=1,3
            DO LPAR=1,3
              V0ERR(IPAR,JPAR) = V0ERR(IPAR,JPAR) +
     &              V0ERR(KPAR,LPAR)*UDERR(KPAR,IPAR)*UDERR(LPAR,JPAR)
            END DO
          END DO
        END DO
C
C  Also build up the correlation terms
C
        DO JPAR=1,3
          V0ERR(IPAR,JPAR) = UDERR(JPAR,IPAR)*V0ERR(JPAR,JPAR)
          V0ERR(JPAR,IPAR) = V0ERR(IPAR,JPAR)
        END DO
      END DO
C
C  Done
C
      RETURN
      END
