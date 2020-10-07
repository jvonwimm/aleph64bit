      SUBROUTINE SPATPR(LUN)
C----------------------------------------------------------------------
C! Print the SPAT bank
C!
C!    Author:     H. Meinhard       13-Jan-1988
C!
C!    Input:      - LUN       /I    logical unit for output
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "sanamc.h"
#include "spatjj.h"
#include "spcpjj.h"
#include "ssppjj.h"
C LC is the maximal number of patch corners
      PARAMETER (LC=8)
      CHARACTER*1 CAB(2)
      DATA CAB / 'A', 'B' /
#include "bmacro.h"
C----------------------------------------------------------------------
C link to banks
      KSPAT = IW(NASPAT)
      KSPCP = IW(NASPCP)
      KSSPP = IW(NASSPP)
      IF (KSPAT.EQ.0 .OR. KSPCP.EQ.0 .OR. KSSPP.EQ.0)       GOTO 901
C
C if banks exist, but no entries, print corresponding message
      IF (LROWS(KSPAT) .EQ. 0) THEN
        WRITE (LUN,500)
      ELSE
        WRITE (LUN,510)
        DO 300 IPAT = 1, LROWS(KSPAT)
          NWIR = ITABL(KSPAT,IPAT,JSPANW)
          WRITE (LUN,520) IPAT,CAB(ITABL(KSPAT,IPAT,JSPASI)),
     +      ITABL(KSPAT,IPAT,JSPANC),(RTABL(KSPAT,IPAT,JSPAAX+I-1),
     +      I=1,LC),NWIR,(ITABL(KSPCP,IPAT,JSPCPC+I-1),I=1,NWIR)
          WRITE (LUN,525) (RTABL(KSPAT,IPAT,JSPAAY+I-1),I=1,LC)
  300   CONTINUE
      ENDIF
      GOTO 999
C----------------------------------------------------------------------
  901 IF (KSPAT .EQ. 0) WRITE (LUN,530) 'SPAT'
  902 IF (KSPCP .EQ. 0) WRITE (LUN,530) 'SPCP'
  903 IF (KSSPP .EQ. 0) WRITE (LUN,530) 'SSPP'
      GOTO 999
  999 CONTINUE
      RETURN
C
C Formats
  500 FORMAT (9X,'No patches found.')
  510 FORMAT ('  Nr   Side   # corners',21X,
     +  'coordinates (ax up, ay down)',17X,'# hits',13X,'hits used')
  520 FORMAT (I4,5X,A1,I9,7X,8(F7.4,1X),2X,I1,2X,9(I3,1X))
  525 FORMAT (26X,8(F7.4,1X))
  530 FORMAT (' --> SPATPR: Bank ',A4,' not found')
      END
#endif
