      SUBROUTINE LKSHOW (JTYP)
C -----------------------------------------------------------
C - Input  : JTYP /I = LKGTEVT return code
C ----------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "lkevt.h"
#include "lkfil.h"
      DATA NEVEH,NRUNR,NRUNH/3*0/
C -----------------------------------------------------------
      IF (NEVEH.EQ.0) THEN
         NEVEH = NAMIND('EVEH')
         NRUNR = NAMIND('RUNR')
         NRUNH = NAMIND('RUNH')
      ENDIF
C
      ITYP = JTYP
      IF (ITYP .EQ. 0) CALL LKGTEVR (ITYP)
C
      IF(ITYP .EQ.1) THEN
        IEVTR = IW(IW(NEVEH)+6)
        IRUNTR = IW(IW(NEVEH)+2)
        WRITE(LOUT,*)' EVENT RECORD,    event:',IEVTR,' , run:',IRUNTR
      ELSEIF(ITYP.EQ.2) THEN
        IRUNTR = IW(IW(NRUNR)+2)
        WRITE(LOUT,*)' RUN RECORD,    run:',IRUNTR
      ELSEIF(ITYP.EQ.3) THEN
        WRITE(LOUT,*)' UNKNOWN RECORD'
      ENDIF
      RETURN
      END
#endif
