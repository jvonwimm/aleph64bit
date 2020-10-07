      SUBROUTINE YSTADD(IROW,ITK,IER)
C------------------------------------------------------------------------------
C!  Add a track to the work bank to be fitted
C
C       Author:  D.Casper 04/07/96
C       
C   Input:  IROW = row of work bank to load
C           ITK  = track number (row) in VSTL
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
        DATA NYSTL/0/
        SAVE NYSTL
C
#include "bmacro.h"
C
        IF (NYSTL.EQ.0) THEN
            NYSTL = NAMIND('YSTL')
        ENDIF
C
C+  Check presence of work banks
C
        IF (INDYSTL.LE.0 .OR. INDYSTM.LE.0) THEN
            IER = -1
            GOTO 999
        ENDIF
C
C+  Check validity of input
C
        IF (IROW.GT.IW(INDYSTL+LMHROW)) THEN
            IER = 1
            GOTO 999
        ENDIF
C
C+  Check presence of track bank
C
        KYSTL = IW(NYSTL)
        IF (KYSTL.LE.0) THEN
            IER = -1
            GOTO 999
        ENDIF
C
C+  Check validity of input
C
        IF (ITK.GT.MIN(LROWS(KYSTL),MAX_TRACKS))THEN
            IER = 1
            GOTO 999
        ENDIF
C
C+  Update the work banks
C
        IYSTL = KROW(KYSTL,ITK)
C+  Put track number into work bank
        IW(KROW(INDYSTL,IROW)+1) = ITK
C+  Store mask for this track
        IWRD = (ITK-1)/32 + 1
        IF (IROW.EQ.1) THEN
            CALL VZERO(IW(KROW(INDYSTM,1)+1),4)
            IW(KROW(INDYSTM,1)+IWRD) = IBSET(0,MOD(ITK-1,32))
        ELSE
            CALL UCOPY(IW(KROW(INDYSTM,IROW-1)+1),
     &          IW(KROW(INDYSTM,IROW)+1),4)
            IW(KROW(INDYSTM,IROW)+IWRD) 
     &          = IBSET(IW(KROW(INDYSTM,IROW)+IWRD),MOD(ITK-1,32))
        ENDIF
        IER = 0
 999    CONTINUE
        RETURN
        END
#endif
