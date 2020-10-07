      SUBROUTINE YSVADD(IROW,NR,IVX,IER)
C------------------------------------------------------------------------------
C!  Add a vertex to the work bank to be fitted
C
C       Author:  D.Casper 04/07/96
C       
C   Input:  IROW = row of work bank to load
C           NR   = number of YSVX bank with vertex to be added
C           IVX  = vertex number (row) in this bank
C   Output:
C           IER = -1 Bank missing
C           IER = 1  Illegal row
C           IER = 0  Normal
C
C   Procedure:  Load the vertex into the requested row of the work bank
C------------------------------------------------------------------------------
#ifndef DOC
#include "ysvxjj.h"
#include "ysvwrk.h"
#include "bcs.h"
C
#include "bmacro.h"
C
C+  Check presence of work banks
C
        IF (INDYSVW.LE.0 .OR. INDYSEW.LE.0 .OR.
     &      INDYSTW.LE.0 .OR. INDYS0W.LE.0 .OR. INDYSCW.LE.0) THEN
            IER = -1
            GOTO 999
        ENDIF
C
C+  Check validity of input
C
        IF (IROW.GT.IW(INDYSVW+LMHROW)) THEN
            IER = 1
            GOTO 999
        ENDIF
C
C+  Check presence of vertex bank
C
        KYSVX = NLINK('YSVX',NR)
        IF (KYSVX.LE.0) THEN
            IER = -1
            GOTO 999
        ENDIF
C
C+  Check validity of input
C
        IF (IVX.GT.LROWS(KYSVX))THEN
            IER = 1
            GOTO 999
        ENDIF
C
C+  Update the work banks
C
        IYSVX = KROW(KYSVX,IVX)
C+  Copy vertex and error matrix
        CALL UCOPY(RW(IYSVX+JYSVVX),RW(KROW(INDYSVW,IROW)+1),3)
        CALL UCOPY(RW(IYSVX+JYSVVM),RW(KROW(INDYSEW,IROW)+1),6)
C+  Update bit masks
        IF (IROW.EQ.1) THEN
          DO IWRD = 1, 3
            IW(KROW(INDYSTW,1)+IWRD) = IW(IYSVX+JYSVTM+IWRD-1)
          ENDDO
          IW(KROW(INDYSCW,1)+1) = IW(IYSVX+JYSVCM)
          IW(KROW(INDYS0W,1)+1) = IW(IYSVX+JYSV0M)
        ELSE
          DO IWRD = 1, 3
            IW(KROW(INDYSTW,IROW)+IWRD) =
     &         IOR(IW(KROW(INDYSTW,IROW-1)+IWRD),
     &         IW(IYSVX+JYSVTM+IWRD-1))
          ENDDO
          IW(KROW(INDYS0W,IROW)+1) = 
     &         IOR(IW(KROW(INDYS0W,IROW-1)+1),
     &         IW(IYSVX+JYSV0M))
          IW(KROW(INDYSCW,IROW)+1) = 
     &         IOR(IW(KROW(INDYSCW,IROW-1)+1),
     &         IW(IYSVX+JYSVCM))
        ENDIF
        IER = 0
 999    CONTINUE
        RETURN
        END
#endif
