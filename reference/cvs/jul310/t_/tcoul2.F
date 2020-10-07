      SUBROUTINE TCOUL2 (NPADS, RPULS, PRFS2, RPHI, IERY, WSTD2)
C
C----------------------------------------------------------------------
C!  TPC coordinates finding : unweighted least-squares estimation of
C!  amplitude and position, Gaussian pulse (F). Uses all NPADS pads.
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
C!    I4/  IERY     : error flag = 161: < 3 pads;
C!                                 162: denominator = 0;
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
#include "tparac.h"
#include "tcocom.h"
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
         IERY = 161
         RETURN
C
C *** 3 pads : special formula with PRFS2 given by parametrization.
C
      ELSE IF (NPADS .EQ. 3) THEN
C
         RPHI = 2.0 + 0.5*PRFS2*
     1       ( ALOG(RPULS(3)) - ALOG(RPULS(1)) )
         RETURN
C
C *** General case, more than 3 pads with PRFS2 computed.
C
      ELSE
C
         DOSTD = .TRUE.
         CALL TCOWAV (NPADS, RPULS, DOSTD, WAVG, WSTD2, IERY)
         IF (IERY .NE. 0) RETURN
C
         R4T1 = 0.0
         R4U1 = 0.0
         DO 20 IPD = 1, NPADS
            R4L = ALOG(RPULS(IPD))
            R4U1 = R4U1 + R4L
            R4T1 = R4T1 + R4L*FLOAT(IPD)
   20    CONTINUE
C
         I4P = NPADS*MOMPH2(NPADS) - MOMPH1(NPADS)*MOMPH1(NPADS)
C
         IF (I4P .EQ. 0) THEN
            IERY = 162
            RETURN
         ENDIF
C
         R4L = 0.5*(  FLOAT(NPADS) * FLOAT(MOMPH3(NPADS))  -
     1                FLOAT( MOMPH1(NPADS) * MOMPH2(NPADS) )    )
         R4L = R4L + WSTD2*(
     1         FLOAT(NPADS)*R4T1 - R4U1*FLOAT(MOMPH1(NPADS))  )
C
         RPHI = R4L / FLOAT(I4P)
         RETURN
C
      ENDIF
      END
#endif
