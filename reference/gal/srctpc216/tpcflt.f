      SUBROUTINE TPCFLT(KINIT,NZERO,AZERO,NPOLE,APOLE,TIME,DRESP)
C-----------------------------------------------------------------------
C!  Compute the response at time TIME of the TPC electronics chain to a
C!  step function at time 0.
C!  To avoid under/over flow problems the internal unit of time is the
C!  micro-sec. So poles and zeros are divided by 10**6, while T is
C!  multiplied by 10**6
C
C  Called from:  TPSHAP
C  Calls:        None
C
C  Inputs:   PASSED:    --KINIT, initialization flag
C                       --NZERO, the number of zeros in the circuit
C                       --AZERO, the values of the zeros
C                       --NPOLE, the number of poles
C                       --APOLE, the values of the poles
C                       --TIME,  the time at which to calculate the
C                                response, given an input step function
C                                at time 0
C  Outputs:  PASSED:    --DRESP, the response at time TIME
C
C  R. Richter, 24.10.83
C-----------------------------------------------------------------------
C  Modifications for APOLLO 10000 ONLY
C  A Rouge   10 Nov 1989
C COMPLEX*16 exists only on APOLLO 10000 -
C
      IMPLICIT COMPLEX(A)
C
      DIMENSION AZERO(NZERO),APOLE(NPOLE),ASF(10), ELGASF(10)
C
      IF ( KINIT .NE. 1 ) GOTO 100
C
C  Initialize
C
      EX10 = ALOG10(EXP(1.))
C  Insure that no two poles have values too close together
C
      DO 20 I = 1,NPOLE-1
         DO 20 K = I+1,NPOLE
C
            IF ( CABS( (APOLE(I) - APOLE(K))/ APOLE(I) ) .GT. 1.E-3 )
     *         GOTO 20
            IF ( ABS( AIMAG(APOLE(I))-AIMAG(APOLE(K)) ) .GT. 1.E-9 )
     *         GOTO 20
            APOLE(K) = APOLE(K) * 1.005
            APOLE(I) = APOLE(I) / 1.005
C
   20 CONTINUE
C
      DO 60 I = 1,NPOLE
C
         ACNUM = CMPLX(1.,0.)
         DO 40 K = 1,NZERO
40          ACNUM = ACNUM * (AZERO(K) - APOLE(I))
C
         ACDEN = CMPLX(1.,0.)
         DO 50 K = 1,NPOLE
 50         IF (K .NE. I) ACDEN = ACDEN * (APOLE(K) - APOLE(I))
C
         ASF(I) = ACNUM / ACDEN
         ELGASF(I) = ALOG10 (CABS(ASF(I)))
C
 60   CONTINUE
C
      RETURN
C                               -------- END OF INITIALIZATION ----
C
C  Begin normal run
C
 100  DRESP = 0
C
C
      DO 150 I = 1,NPOLE
C  If AHADD will underflow skip this pass of the loop
      EPOW = ELGASF(I) - EX10 * CABS (APOLE(I)) * TIME
         IF (EPOW .LT. -40.) GOTO 150
         AHEXP = CEXP(-APOLE(I)*TIME)
         AHADD = ASF(I) * AHEXP
C  As only complex conjugate poles are present the imag. parts
C     cancel and can be left away
         DRESP = DRESP + REAL (AHADD)
 150  CONTINUE
C
      RETURN
      END
