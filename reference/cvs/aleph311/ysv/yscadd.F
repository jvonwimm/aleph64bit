        SUBROUTINE YSCADD(IROW,ITK,IER)
C------------------------------------------------------------------------------
C!  Add an ITC track to the work bank to be fitted
C
C       Author:  D.Casper 04/07/96
C       
C   Input:  IROW = row of work bank to load
C           ITK  = track number (row) in VSCL
C   Output:
C           IER = -1 Bank missing
C           IER = 1  Illegal row
C           IER = 0  Normal
C
C   Procedure:  Load the track into the requested row of the work bank
C------------------------------------------------------------------------------
#ifndef DOC
        PARAMETER (MAX_TRACKS = 128)
#include "ysvwrk.h"
#include "bcs.h"
C
        DATA NYSCL/0/
        SAVE NYSCL
C
#include "bmacro.h"
C
        IF (NYSCL.EQ.0) THEN
            NYSCL = NAMIND('YSCL')
        ENDIF
C
C+  Check presence of work banks
C
        IF (INDYSCL.LE.0 .OR. INDYSCM.LE.0) THEN
            IER = -1
            GOTO 999
        ENDIF
C
C+  Check validity of input
C
        IF (IROW.GT.IW(INDYSCL+LMHROW)) THEN
            IER = 1
            GOTO 999
        ENDIF
C
C+  Check presence of track bank
C
        KYSCL = IW(NYSCL)
        IF (KYSCL.LE.0) THEN
            IER = -1
            GOTO 999
        ENDIF
C
C+  Check validity of input
C
        IF (ITK.GT.MIN(LROWS(KYSCL),MAX_TRACKS))THEN
            IER = 1
            GOTO 999
        ENDIF
C
C+  Update the work banks
C
        IYSCL = KROW(KYSCL,ITK)
C+  Put track number into work bank
        IW(KROW(INDYSCL,IROW)+1) = ITK
C+  Store mask for this track
        IWRD = (ITK-1)/32 + 1
        IF (IROW.EQ.1) THEN
            CALL VZERO(IW(KROW(INDYSCM,1)+1),4)
            IW(KROW(INDYSCM,1)+IWRD) = IBSET(0,MOD(ITK-1,32))
        ELSE
            CALL UCOPY(IW(KROW(INDYSCM,IROW-1)+1),
     &          IW(KROW(INDYSCM,IROW)+1),4)
            IW(KROW(INDYSCM,IROW)+IWRD) 
     &          = IBSET(IW(KROW(INDYSCM,IROW)+IWRD),MOD(ITK-1,32))
        ENDIF
        IER = 0
 999    CONTINUE
        RETURN
        END
#endif
