      subroutine KP6ZFR(IST)
C--------------------------------------------------------------------
C!  BOOK and fill bank KZFR with fragmentation info
C      B. Bloch -Devaux November 1998 for PYTHIA 6.1
C     structure : subroutine
C
C     input     : none
C
C     output    : IST status flag ( 0 means ok)
C     comdecks referenced : PYT6COM
C--------------------------------------------------------------------
#ifndef DOC
#include "pyt6com.h"
C
      REAL*4 ZB(LJNPAR)
      Integer ALTABL
      External ALTABL
      IST = 0
C  Reset ZB storage
      CALL VZERO(ZB,LJNPAR)
C keep fragmentation info
      IF (N7LU.GT.0) CALL KPYZET(ZB)
C
C   Set the ZB value according to KINE numbering, i.e. remove beam part.
C   and transmit z of mother to subsequent heavy baryons and mesons
      IBEA = 0
      DO 27 ITR=1,N7LU
      KS = K7LU(ITR,1)
      KM = K7LU(ITR,3)
C  Give same z to all daughters  of a mother
      IF (KM.GT.IBEA .AND. ZB(KM-IBEA).GT.0. ) ZB(ITR) = ZB(KM-IBEA)
      IF ( KS.EQ.21 .AND. ABS(K7LU(ITR,2)).EQ.11 ) THEN
       IBEA = IBEA +1
      ELSE
       ZB(ITR-IBEA) = ZB (ITR)
      ENDIF
 27   CONTINUE
C
C   Book & fill the bank KZFR with info on fragmentation
C
      NP = N7LU-IBEA
      JKZFR = ALTABL('KZFR',1,NP,ZB,'2I,(F)','E')
      IF(JKZFR.LE.0) IST = -100
      RETURN
      END
#endif
