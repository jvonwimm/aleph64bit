      SUBROUTINE TCOUL3 (NPADS, RPULS, DOSTD, RPHI, PRFS2, IERY)
C
C--------------------------------------------------------------------
C!  TPC coordinates finding : unweighted least-squares estimation of
C!  the 3 parameters of a Gaussian pulse (I). Uses all NPADS pads.
C!
C!  Author:    T. Pun      12-06-86
C!  Modified:  R. Johnson   8-04-88
C!
C!  INPUT :
C!    I4/  NPADS     : # of pads in this cluster, in [2 ... LMXPCL].
C!    R4/  RPULS(ip): p heights for this cluster (in [1 ... 255] !).
C!    L4/  DOSTD    : if .TRUE. computes also PRFS2.
C!
C!  OUTPUT :
C!    R4/  RPHI     : coordinate, in [1..NPADS].
C!    R4/  PRFS2   : width**2 (>0 !) of PRF.
C!                    If = 3 pads, obtained from 3 pads algorithm (B);
C!                    If > 3 pads, computed here, IF DOSTD is .TRUE..
C!    I4/  IERY     : error flag = 191: < 3 pads;
C!                                 192: denominator < eps for r*phi;
C!                                 193: PRFS2 < 0.
C!
C!  MODULE(S) :
C!         TCOGS3   : 2 pads algorithm.
C!
C!  Called by TCOALG
C!
C!-------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "tcocom.h"
C
      PARAMETER (R4EPS = 1.0E-06)
C
      DIMENSION RPULS(NPADS)
      LOGICAL DOSTD
C
      IERY = 0
      RPHI = 0.0
      PRFS2 = 0.0
C
C *** Less than 3 pads : error.
C
      IF (NPADS .LT. 3) THEN
C
         IERY = 191
         RETURN
C
C *** 3 pads exactly : special formula with PRFS2 given by alg (B).
C
      ELSE IF (NPADS .EQ. 3) THEN
C
         CALL TCOGS3 (1, RPULS, R4POS, PRFS2, IERY)
         IF (IERY .NE. 0) RETURN
C
         RPHI = 2.0 + 0.5*PRFS2*
     1         ( ALOG(RPULS(3)) - ALOG(RPULS(1)) )
         RETURN
C
C *** General case, more than 3 pads with PRFS2 computed.
C
      ELSE
C
         R4T1 = 0.0
         R4T2 = 0.0
         R4U1 = 0.0
C
         DO 20 IPD = 1, NPADS
            R4L = ALOG(RPULS(IPD))
            R4U1 = R4U1 + R4L
            R4L = R4L*FLOAT(IPD)
            R4T1 = R4T1 + R4L
            R4L = R4L*FLOAT(IPD)
            R4T2 = R4T2 + R4L
   20    CONTINUE
C
         R4NP = FLOAT(NPADS)
         R4P2 = FLOAT(MOMPH2(NPADS))
         R4211 = FLOAT( NPADS * MOMPH2(NPADS) -
     1                   MOMPH1(NPADS) * MOMPH1(NPADS)  )
         R4312 = R4NP * FLOAT(MOMPH3(NPADS)) -
     1           FLOAT( MOMPH1(NPADS) * MOMPH2(NPADS)  )
         R4422 = R4NP*FLOAT(MOMPH4(NPADS)) - R4P2*R4P2
         R4TPU = R4NP*R4T1 - FLOAT(MOMPH1(NPADS))*R4U1
         R4T2P = R4NP*R4T2 - R4P2*R4U1
C
         R4P = 2.0*(R4211*R4T2P - R4312*R4TPU)
C
         IF (ABS(R4P) .LT. R4EPS) THEN
            IERY = 192
            RETURN
         ENDIF
C
         RPHI = (R4312*R4T2P - R4422*R4TPU) / R4P
C
         IF (DOSTD) THEN
            PRFS2 = (R4312*R4312 - R4422*R4211) / R4P
            IF (PRFS2 .LT. 0.0) THEN
               IERY = 193
            ENDIF
         ENDIF
C
         RETURN
      ENDIF
      END
#endif
