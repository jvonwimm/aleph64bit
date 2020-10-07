      SUBROUTINE DECOS(TETA1,PHI1,TETA2,PHI2,A1,A2,SIGMACOS)
CKEY QPI0DO / INTERNAL
C-----------------------------------------------------------------------
C! Auxiliary to KINEFIT
C    Author   :- Marcello Maggi        27-Jan-1992
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-G,O-Z)
      DTET1=DSIN(TETA1)*A1
      DTET2=DSIN(TETA2)*A2
      DPHI1=A1
      DPHI2=A2
      DERTET1=    DCOS(TETA1)*DCOS(PHI1)*DSIN(TETA2)*DCOS(PHI2) +
     >            DCOS(TETA1)*DSIN(PHI1)*DSIN(TETA2)*DSIN(PHI2) -
     >            DSIN(TETA1)*DCOS(TETA2)
      DERPHI1=-1.D0*DSIN(TETA1)*DSIN(PHI1)*DSIN(TETA2)*DCOS(PHI2) +
     >            DSIN(TETA1)*DCOS(PHI1)*DSIN(TETA2)*DSIN(PHI2)
      DERTET2=    DSIN(TETA1)*DCOS(PHI1)*DCOS(TETA2)*DCOS(PHI2) +
     >            DSIN(TETA1)*DSIN(PHI1)*DCOS(TETA2)*DSIN(PHI2) -
     >            DCOS(TETA1)*DSIN(TETA2)
      DERPHI2=-1.D0*DSIN(TETA1)*DCOS(PHI1)*DSIN(TETA2)*DSIN(PHI2) +
     >            DSIN(TETA1)*DSIN(PHI1)*DSIN(TETA2)*DCOS(PHI2)

      SIGMACOS=SQRT( DERTET1*DERTET1*DTET1*DTET1 +
     >               DERPHI1*DERPHI1*DPHI1*DPHI1 +
     >               DERTET2*DERTET2*DTET2*DTET2 +
     >               DERPHI2*DERPHI2*DPHI2*DPHI2 )
      RETURN
      END
