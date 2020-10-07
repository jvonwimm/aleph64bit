      SUBROUTINE TROT(TERR2,T1,J1,NU,HAT,ERR2)
CKEY   QFNDIP  / INTERNAL
C ----------------------------------------------------------------------
C! Rotate the track error matrix from the phi-theta coordinate
C  system to the E,M system.  E and M are
C  defined relative to the jet axis.  This also returns the unit vectors
C  the various directions. Dave Brown, 9-8-93
C  Called from FINDIP
C ----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Inputs
C
      REAL TERR2(3),T1(3),J1(3),NU
C
C  Outputs
C
      REAL HAT(3,4)
      REAL ERR2(4)
C
C  Local variables
C
      INTEGER ICOR
      REAL COSE,SINE,COSA,SINA
      REAL THETA
      REAL NORM
C
C  Minimum possible error of 25 microns
C
      REAL MINERR2
      DATA MINERR2/6.25E-6/
C ----------------------------------------------------------------------
C  Find the theta and
C  phi direction unit vector from the track
C  direction unit vector, and the ALEPH conventions.
C
      NORM = SQRT(T1(1)**2+T1(2)**2)
      HAT(1,2) = -T1(2)/NORM
      HAT(2,2) =  T1(1)/NORM
      HAT(3,2) = 0.0
      HAT(1,1) = -T1(1)*T1(3)/NORM
      HAT(2,1) = -T1(2)*T1(3)/NORM
      HAT(3,1) =  NORM
      ERR2(2) = MAX(TERR2(1),MINERR2)
      ERR2(1) = MAX(TERR2(2),MINERR2)
C
C  Find the E and M directions, using the jet direction
C
      IF(NU.GT.0.0)THEN
        NORM = SIN(NU)
        HAT(1,3) = (T1(2)*J1(3) - T1(3)*J1(2))/NORM
        HAT(2,3) = (T1(3)*J1(1) - T1(1)*J1(3))/NORM
        HAT(3,3) = (T1(1)*J1(2) - T1(2)*J1(1))/NORM
        HAT(1,4) = -T1(2)*HAT(3,3)+ T1(3)*HAT(2,3)
        HAT(2,4) = -T1(3)*HAT(1,3)+ T1(1)*HAT(3,3)
        HAT(3,4) = -T1(1)*HAT(2,3)+ T1(2)*HAT(1,3)
C
C  Solve for the rotation angle between phat, that, and ehat
C
        COSE = 0.0
        SINE = 0.0
        DO ICOR=1,3
          COSE = COSE + HAT(ICOR,3)*HAT(ICOR,2)
          SINE = SINE + HAT(ICOR,3)*HAT(ICOR,1)
        END DO
C
C  Get the E and M direction errors; these aren't diagonalized!!
C
        ERR2(3) = MAX(TERR2(1)*COSE**2 + TERR2(2)*SINE**2 +
     &        2.*TERR2(3)*COSE*SINE,MINERR2)
        ERR2(4) = MAX(TERR2(2)*COSE**2 + TERR2(1)*SINE**2 -
     &        2.*TERR2(3)*COSE*SINE,MINERR2)
      ELSE
C
C  Just copy the other vectors
C
        DO ICOR=1,3
          HAT(ICOR,3) = HAT(ICOR,1)
          HAT(ICOR,4) = HAT(ICOR,2)
        END DO
        ERR2(3) = ERR2(1)
        ERR2(4) = ERR2(2)
      END IF
      RETURN
      END
