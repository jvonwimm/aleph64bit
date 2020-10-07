      SUBROUTINE SCLUPR(LUN)
C----------------------------------------------------------------------
C! Print the SCLU bank
C!
C!    Author:     H. Meinhard       06-Jan-1988
C!
C!    Input:      - LUN       /I    logical unit for output
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sanamc.h"
#include "sccpjj.h"
#include "sclujj.h"
#include "ssbpjj.h"
      CHARACTER*1 CAB(2)
      DATA CAB / 'A', 'B' /
#include "bmacro.h"
C----------------------------------------------------------------------
C link to banks
      KSCLU = IW(NASCLU)
      KSCCP = IW(NASCCP)
      KSSBP = IW(NASSBP)
      IF (KSCLU.EQ.0 .OR. KSCCP.EQ.0 .OR. KSSBP.EQ.0)       GOTO 901
C
C if banks exist, but no entries, print corresponding message
      IF (LROWS(KSCLU) .EQ. 0) THEN
        WRITE (LUN,500)
      ELSE
        WRITE (LUN,510)
        DO 300 ICL = 1, LROWS(KSCLU)
          NWIR = ITABL(KSCLU,ICL,JSCLNW)
          WRITE (LUN,520) ICL,CAB(ITABL(KSCLU,ICL,JSCLSI)),
     +      ITABL(KSCLU,ICL,JSCLOR),ITABL(KSCLU,ICL,JSCLSC),
     +      RTABL(KSCLU,ICL,JSCLTL),RTABL(KSCLU,ICL,JSCLTU),
     +      (ITABL(KSCCP,ICL,I),I=1,NWIR)
  300   CONTINUE
      ENDIF
      GOTO 999
C----------------------------------------------------------------------
  901 IF (KSCLU .EQ. 0) WRITE (LUN,530) 'SCLU'
      IF (KSCCP .EQ. 0) WRITE (LUN,530) 'SCCP'
      IF (KSSBP .EQ. 0) WRITE (LUN,530) 'SSBP'
      GOTO 999
  999 CONTINUE
      RETURN
C
C Formats
  500 FORMAT (9X,'No clusters found.')
  510 FORMAT ('  Nr   Side   Orient.   Sector   lower tan theta   ',
     +        'upper tan theta     hits used')
  520 FORMAT (I4,5X,A1,I8,I10,E17.4,E18.4,7X,3(I3,1X))
  530 FORMAT (' --> SCLUPR: Bank ',A4,' not found')
      END
#endif
