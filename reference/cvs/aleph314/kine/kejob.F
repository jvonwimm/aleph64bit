      SUBROUTINE KEJOB (LWRT,MXEVT)
C ----------------------------------------------------------------
C - F.Ranjard - 870505
C! KINGAL end of job
CKEY KINE KINGAL FILL BANK /  INTERNAL
C   close output file if it exists and print BOS statistics
C   B.Bloch   - 9812   add bank KSEC to end of run bank list
C
C - structure: SUBROUTINE program
C              User Entry Name: KEJOB
C              External References: ALRUNE/BKENRU/BKENJO(ALEPHLIB)
C                                   NAMIND/BWRITE/BOSTA(BOS77)
C              Comdecks referenced: BCS, BMACRO
C
C - usage   : CALL KEJOB (LWRT,MXEVT)
C - input   : LWRT   = output file logical unit
C             MXEVT  = # of events in the run
#ifndef DOC
      SAVE
      INTEGER ALRUNE
      EXTERNAL NAMIND
#include "bcs.h"
#include "bmacro.h"
C ---------------------------------------------------------------
C - Create RUNE bank
      JRUNH = IW(NAMIND('RUNH'))
      IEXP = IW (JRUNH + 1)
      IRUN = IW (JRUNH + 2)
      IRTY = IW (JRUNH + 3)
      IRUNE = ALRUNE ( IRUN,IEXP,IRTY,MXEVT )
      CALL BLIST (IW,'C=','KSECRUNE')
      IF (LWRT.NE.0) THEN
C
C Write end of run bank :
         CALL BWRITE (IW,LWRT,'C')
C
C - Close output file
         CALL BWRITE (IW,LWRT,'0')
      ENDIF
C
C - BOS statistics
      CALL BOSTA
C
      END
#endif
