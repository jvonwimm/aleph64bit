      SUBROUTINE QWRSEVT
CKEY FILL INTERNAL
C-----------------------------------------------------------------------
C! Fill internal banks for further writing of SEVT cards
C!                                               J. Boucrot 29-01-2000
C!called by the user in QUEVNT for selected events
C-----------------------------------------------------------------------
#ifndef DOC
      PARAMETER ( NBROW = 200 )
#include "qcde.h"
#include "qmacro.h"
C-----------------------------------------------------------------------
      IF (KRUN.LE.0.OR.KEVT.LE.0) GO TO 999
C
C Internal bank QWSE : NR       = run number
C                      Ncols    = 1     
C                      Nrows    containing selected event numbers
C
C Test if the bank exists; if not book it ;
C if no room left, enlarge it by creating NBROW additional rows
C
      JBANK=NLINK('QWSE',KRUN) 
      IF (JBANK.LE.0) THEN
         LENB=NBROW+LMHLEN
         IOLRO=0 
      ELSE
         IOLRO=LROWS(JBANK)
         IF (LFRROW(JBANK).GT.1) GO TO 10
         LENB=IW(JBANK)+NBROW
      ENDIF
C Book or enlarge 'QWSE', NR=KRUN :
      CALL AUBOS('QWSE',KRUN,LENB,JBANK,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      IW(JBANK+LMHCOL)=1
      IW(JBANK+LMHROW)=IOLRO
C
C Fill one more row for this bank with event number:
C
 10   KBANK=KNEXT(JBANK)  
      IW(KBANK+1)=KEVT
      IW(JBANK+LMHROW)=IOLRO+1 
C
 999  RETURN
      END
#endif
