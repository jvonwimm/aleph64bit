      SUBROUTINE EIDEDX(NTR,REST)
C-------------------------------------------------------
C!  GET R5 ESTIMATOR FOR ELECTRON IDENTIFICATION
C!
C!  AUTHOR   : D. PALLIN
C!  MODIFIED : F.Ranjard - 951201
C              call TPDHYP('WIRE', instead of TIDHYP
C?
C!======================================================================
#ifndef DOC
#include "etpinf.h"
#include "rconds.h"
      PARAMETER (RMASS=.511E-3)
C RMASS : ELECTRON MASS
      N=1
      Q=1
      CALL TPDHYP('WIRE',NTR,FIELRC,N,RMASS,Q,RI,XNS,TL,RIEXP,SIGM,
     &            IER)
      IF(IER.EQ.0)THEN
         REST=((RI-RIEXP)/SIGM)
      ELSE
         REST=1000.
      ENDIF
 999  RETURN
      END
#endif
