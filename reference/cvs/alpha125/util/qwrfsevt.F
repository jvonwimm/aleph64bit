      SUBROUTINE QWRFSEVT(SEVTF)
CKEY FILL /INTERNAL
C-----------------------------------------------------------------------
C! Write SEVT cards file from lists of events selected by the user.
C
C     Author: J. Boucrot      27-Jan-2000
C
C     Input  : QWSE banks
C
C     Called by the user in QUTERM
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      PARAMETER ( LUNOU = 93 )
      CHARACTER*(*) SEVTF
#include "bmacro.h"
C-----------------------------------------------------------------------
C First check that at least one 'QWSE' bank exists:
      IF (IW(NAMIND('QWSE')).EQ.0) GO TO 999
C Compress these banks:
      CALL AUBPRS('QWSE')
      LLF=LNBLNK(SEVTF)
C Open the output file for writing:
      CALL AOPENW(LUNOU,SEVTF(1:LLF),'CARDS','DISK',IERO)
      IF (IERO.GT.0) THEN
         WRITE (IW(6),1234) SEVTF(1:LLF)       
         GO TO 999
      ENDIF
C
C Implicit loop on all 'QWSE' banks:
C
      JQWSE=NAMIND('QWSE')+1
 10   JQWSE=IW(JQWSE-1)  
      IF (JQWSE.EQ.0) GO TO 500
C Write the SEVT cards corresponding to this bank:
      NURUN=IW(JQWSE-2)
      CALL QWRQWSE(LUNOU,NURUN,IERS)      
      IF (IERS.GT.0) THEN
         WRITE (IW(6),1235) NURUN       
         GO TO 999
      ENDIF
      GO TO 10
C
C all cards written: close output file 
C 
 500  CALL ACLOSE(LUNOU,IERB)
C
 999  RETURN
 1234 FORMAT (/' _QWRFSEVT Error when opening file : ',(A)) 
 1235 FORMAT (/' _QWRFSEVT Error when writing SEVT for run :',i9)
C
      END
#endif
