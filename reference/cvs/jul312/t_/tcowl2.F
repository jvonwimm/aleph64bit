      SUBROUTINE TCOWL2 (NPADS, RPULS, PRFS2, RPHI, IERY, WSTD2)
C
C--------------------------------------------------------------------
C!  TPC coordinates finding : weighted least-squares estimation of
C!  amplitude and position, Gaussian pulse (G). Uses all NPADS pads.
C!
C!  Author:    T. Pun      12-06-86
C!  Modified:  R. Johnson   8-04-88
C!
C!  INPUT :
C!    I4/  NPADS     : # of pads in this cluster, in [2 ... LMXPCL].
C!    R4/  RPULS(ip): p heights for this cluster (in [1 ... 255] !).
C!    R4/  PRFS2   : width**2 (>0 !) of PRF.
C!
C!  OUTPUT :
C!    R4/  RPHI     : coordinate, in [1..NPADS].
C!    I4/  IERY     : error flag = 171: < 3 pads.
C!                                172: denominator < eps.
C!    R4/  WSTD2   : width**2 (>0 !) of PRF.
C!
C!  MODULE(S) :
C!         TCOWAV   : computes weighted standard-deviation.
C!
C!  Called by TCOALG
C!
C!-------------------------------------------------------------------
#ifndef DOC
C
      PARAMETER (R4EPS = 1.0E-06)
C
      DIMENSION RPULS(NPADS)
      LOGICAL DOSTD
C
      IERY = 0
      RPHI = 0.0
C
C *** Less than 3 pads : error.
C
      IF (NPADS .LT. 3) THEN
C
         IERY = 171
         RETURN
C
C *** 3 pads : special formula with PRFS2 given by parametrization.
C
      ELSE IF (NPADS .EQ. 3) THEN
C
         R412 = RPULS(1) * RPULS(2)
         R423 = RPULS(2) * RPULS(3)
         R413 = 4.*RPULS(1) * RPULS(3)
C
         RP12 = 1.5 + PRFS2*( ALOG(RPULS(2))
     1                       - ALOG(RPULS(1)) )
C
         RP23 = 2.5 + PRFS2*( ALOG(RPULS(3))
     1                       - ALOG(RPULS(2)) )
C
         RP13 = 2.0 + 0.5*PRFS2*
     1          ( ALOG(RPULS(3)) - ALOG(RPULS(1)) )
C
         RPHI = (R412*RP12 + R423*RP23 + R413*RP13) /
     1                                  (R412 + R423 + R413)
         RETURN
C
C *** General case, more than 3 pads with PRFS2 computed.
C
      ELSE
         DOSTD = .TRUE.
         CALL TCOWAV (NPADS, RPULS, DOSTD, WAVG, WSTD2, IERY)
         IF (IERY .NE. 0) RETURN
C
         R4C1 = 0.
         R4P1 = 0.
         R4P2 = 0.
         R4P3 = 0.0
         R4T1 = 0.0
         R4U1 = 0.0
C
         DO 20 IPD = 1, NPADS
            R4C = RPULS(IPD)
            R4C1 = R4C1 + R4C
            R4U = ALOG(R4C) * R4C
            R4U1 = R4U1 + R4U
            R4T1 = R4T1 + R4U*FLOAT(IPD)
            R4C = FLOAT(IPD)*R4C
            R4P1 = R4P1 + R4C
            R4C = R4C*FLOAT(IPD)
            R4P2 = R4P2 + R4C
            R4C = R4C*FLOAT(IPD)
            R4P3 = R4P3 + R4C
 20      CONTINUE
C
         R412 = R4C1*R4P2 - R4P1*R4P1
C
         IF (ABS(R412) .LT. R4EPS) THEN
            IERY = 172
            RETURN
         ENDIF
C
         R4U = 0.5*( R4C1*R4P3 - R4P1*R4P2 )
         R4U = R4U + WSTD2*( R4C1*R4T1 - R4U1*R4P1 )
C
         RPHI = R4U / R412
         RETURN
      ENDIF
      END
#endif
