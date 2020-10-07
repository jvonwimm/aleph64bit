*DK ascrun
      SUBROUTINE ASCRUN
C --------------------------------------------------------------
C - F.RANJARD - 860123
C! Close the run
C - Close the run : write last record of the run
C - Called from     QNEXT                      from this .HLB
C - Calls           USCRUN, ASWRTP             dummy on this .HLB
C
C--------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "kincom.h"
#include "jqcom.h"
      INTEGER ALRUNE
C -------------------------------------------------------------
C
C - Call user routine
      CALL USCRUN
C
C - Build  the 'RUNE' bank if not there
      IF (IW(NARUNE) .EQ. 0) THEN
C        Get run type from RUNH bank
        IF (IW(NARUNH).NE.0) THEN
          LRTYP = IW (IW(NARUNH) + 3)
        ELSE
          LRTYP = IPROKI
        ENDIF
        JRUNE = ALRUNE (IRUNJO,IEXPJO,LRTYP,NEVTJO)
        IF (JRUNE.EQ.0) THEN
          CALL ALTELL ('ASCRUN: not enough space to book RUNE ',1,
     &                   'RETURN')
        ENDIF
        CALL BLIST (IW,'C=','RUNE')
      ENDIF
C
C - Write last record on tape
      IF (MSAVJO.GT.0) THEN
         CALL ASWRTP ('C')
      ENDIF
      IF (IPRIJO(14).EQ.1) CALL AUBLIS ('C')
C
      RETURN
      END
#endif
