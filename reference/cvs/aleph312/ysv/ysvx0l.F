      SUBROUTINE YSVX0L(IER)
C--------------------------------------------------------------------
C! Create YSVX/0 and load primary vertex
C
C       Author: D. Casper  1-jul-1996
C   
C       Returns:  IER = 0 if vertex found
C                 IER = -1 if no vertex found
C                 IER = 1 if no room in BOS
C
C       Input bank: PYER    Primary vertex
C
C       Output bank:YSVX/0  Copy of primary in internal format
C
C--------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "pyerjj.h"
#include "ysvxjj.h"
C
      DATA NPYER/0/
      SAVE NPYER
C
#include "bmacro.h"
C
      IER = -1
      IF (NPYER .EQ. 0) NPYER = NAMIND('PYER')
C
C+  Find PYER bank
C
      KPYER = IW(NPYER)
      IF (KPYER .EQ. 0) GOTO 999
C
C+  Find primary vertex in PYER
C
      IV = 1
      DO WHILE (IV.LE.LROWS(KPYER).AND.ITABL(KPYER,IV,JPYETY).NE.1)
        IV = IV + 1
      ENDDO
      IF (IV .GT. LROWS(KPYER)) GOTO 999
C
C+  Create YSVX/0
C
      CALL AUBOS('YSVX',0,LMHLEN+LYSVXA,KYSVX,IGARB)
      IF (IGARB.EQ.1) THEN
        KPYER = IW(NPYER)
      ELSE IF (IGARB.EQ.2) THEN
        IER = 1
        GOTO 999
      ENDIF
      IW(KYSVX+LMHCOL) = LYSVXA
      IW(KYSVX+LMHROW) = 1
C
C+  Store primary
C
      CALL UCOPY(RW(KROW(KPYER,IV)+JPYEVX),RW(KYSVX+LMHLEN+JYSVVX),3)
      CALL UCOPY(RW(KROW(KPYER,IV)+JPYEVM),RW(KYSVX+LMHLEN+JYSVVM),6)
      RW(KYSVX+LMHLEN+JYSVC2) = RTABL(KPYER,IV,JPYEC2)
      IW(KYSVX+LMHLEN+JYSVDF) = ITABL(KPYER,IV,JPYEDF)
C
      IER = 0
C
 999  CONTINUE
      RETURN
      END
#endif
