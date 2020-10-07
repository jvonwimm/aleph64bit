      INTEGER FUNCTION ALRUNE ( NRUN,NEXPE,IRTYP,MEVT )
C --------------------------------------------------------------------
C! Build the RUNE bank
C - F.Ranjard - 861003
C - Input :  NRUN   = run #
C            NEXPE  = experiment #
C            IRTYP  = run type
C            MEVT   = number of events in the run
C - Output : ALRUNE = RUNE bank index
C                     0 means not enough space to book the bank
C  ----------------------------------------------------------
#ifndef DOC
#include "bcs.h"
C   LRUNE  = 'RUNE' bank length
C   NCENT  = century #
      PARAMETER (LRUNE = 6, NCENT = 19)
C ----------------------------------------------------------------------
C
C - Book 'RUNE',NR=0 with length LRUNE
      CALL AUBOS ('RUNE',0,LRUNE,JRUNE,IGARB)
      IF (JRUNE.EQ.0) GOTO 999
      CALL BKFMT ('RUNE','(I)')
C
C - Get date and time of end of run
      CALL DATIME (JDAT,JTIM)
C
      IW (JRUNE+1) = NEXPE
      IW (JRUNE+2) = NRUN
      IW (JRUNE+3) = IRTYP
      IW (JRUNE+4) = JDAT
      IW (JRUNE+5) = JTIM
      IW (JRUNE+6) = MEVT
C
 999  CONTINUE
      ALRUNE = JRUNE
      RETURN
      END
#endif
