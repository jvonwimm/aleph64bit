      SUBROUTINE TCOWAV (NPADS, RPULS, DOSTD, WAVG, WSTD2, IERY)
C
C--------------------------------------------------------------------
C! TPC coordinates finding : weighted avg and standard-dev**2, alg (A).
C! Uses all NPADS pads.
C!
C!  Author:    T. Pun      12-06-86
C!  Modified:  R. Johnson   8-04-88
C!
C!  INPUT :
C!    I4/  NPADS     : # of pads in this cluster, in [2 ... LMXPCL].
C!    R4/  RPULS(ip): p heights for this cluster (in [1 ... 255] !).
C!    L4/  DOSTD    : if .TRUE. computes also WSTD2, otherwise not.
C!
C!  OUTPUT :
C!    R4/  WAVG      : weighted-average, in [1..NPADS].
C!    R4/  WSTD2     : weighted standard-deviation**2; not modified if
C!                     DOSTD = .FALSE.
C!    I4/  IERY      : error flag = 111: WSTD2 < eps (ie. <0 or small).
C!
C!  Called by TCOALG
C!
C-------------------------------------------------------------------
#ifndef DOC
C
      PARAMETER (R4EPS = 1.0E-06)
C
      INTEGER IPUTO
      DIMENSION RPULS(NPADS)
      LOGICAL DOSTD
C
      IERY = 0
      PUTO = 0.
C
      IF (DOSTD) THEN
         R4S1 = 0.
         R4S2 = 0.
         DO 10 IP = 1, NPADS
            PUTO = PUTO + RPULS(IP)
            R4A  = RPULS(IP)*FLOAT(IP)
            R4S1 = R4S1 + R4A
            R4A  = R4A*FLOAT(IP)
            R4S2 = R4S2 + R4A
   10    CONTINUE
C
         R4D = 1.0 / PUTO
         WAVG = R4S1 * R4D
         WSTD2 = R4S2*R4D - WAVG*WAVG
C
         IF (WSTD2 .LT. R4EPS) IERY = 111
C
      ELSE
C
         R4S1 = 0.
         DO 20 IP = 1, NPADS
            PUTO = PUTO + RPULS(IP)
            R4S1 = R4S1 + RPULS(IP)*FLOAT(IP)
   20    CONTINUE
C
         WAVG  = R4S1 / PUTO
C
      ENDIF
C
      RETURN
      END
#endif
