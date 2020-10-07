      SUBROUTINE TCQGS3 (IPADA, RPULS, QCO, IERY)
C--------------------------------------------------------------------
C!  Compute coordinate charge assuming Gaussian; Based on TCOGS3
C!
C!  Author:    T. Pun      12-06-86
C!  Modified:  R. Johnson   8-04-88
C!             D. Casper    5-04-95 Create TCQGS3 from TCOGS3
C!
C!  INPUT :
C!    I4/  IPADA      : indice of 1st pad of the three, in [1 .. NPADS].
C!    R4/  RPULS(ip): p heights for this cluster (in [1 ... 255] !).
C!
C!  OUTPUT :
C!    R4/  QCO     : area under Gaussian = coordinate charge
C!    I4/  IERY     : error flag = 121: negative logarithm.
C!
C!  Called by TCOALG
C!
C?
C!===================================================================
#ifndef DOC
C
      DIMENSION RPULS(*)
C
      IERY = 0
      RPHI = 0.0
      PRFS2 = 0.0
C
      PUL1 = RPULS(IPADA)
      PUL2 = RPULS(IPADA+1)
      PUL3 = RPULS(IPADA+2)
C
      R4L1 = ALOG(PUL2) - ALOG(PUL1)
      R4L  = R4L1 + ALOG(PUL2) - ALOG(PUL3)
C
      IF (R4L .LT. 0.0) THEN
         IERY = 121
         RETURN
      ENDIF
C
      R4L = 1.0 / R4L
      PRFS2 = R4L
      RPHI = FLOAT(IPADA) + 0.5 + R4L*R4L1
      QNORM = PUL1 * EXP((FLOAT(IPADA) - RPHI)**2/(2.*PRFS2))
      QCO = QNORM * SQRT(2.*3.14159 * PRFS2)
      RETURN
      END
#endif
