      SUBROUTINE QWEVNT
CKEY WRITE EVENTS /USER
C----------------------------------------------------------------------
C! print all tracks
C called from user routines
C                                                   H.Albrecht 20.09.88
C----------------------------------------------------------------------
#ifndef DOC
C
#include "qcde.h"
#include "qmacro.h"
C----------------------------------------------------------------------
      CALL QWHEAD ('1','_QWEVNT_ Event listing')
C
      DO 10 L=KSREV,KSMCV
   10 CALL QWSEC (L,'0')
C
      DO 20 L=KSOVT,KSMCT-1
   20 CALL QWSEC (L,'0')
C
      CALL QWNEWTR
C
      DO 30 ITK=KFMCT,KLMCT
   30 IF (KNMOTH(ITK) .EQ. 0)  CALL QWTREE (ITK,'0')
C
      END
#endif
