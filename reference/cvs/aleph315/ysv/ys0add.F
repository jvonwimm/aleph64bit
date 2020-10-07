      SUBROUTINE YS0ADD(IROW,IV0,IER)
C------------------------------------------------------------------------------
C!  Add a V0 to the work bank to be fitted
C
C       Author:  D.Casper 04/07/96
C       
C   Input:  IROW = row of work bank to load
C           ITK  = V0 number (row) in VS0L
C   Output:
C           IER = -1 Bank missing
C           IER = 1  Illegal row
C           IER = 0  Normal
C
C   Procedure:  Load the V0 into the requested row of the work bank
C------------------------------------------------------------------------------
#ifndef DOC
        PARAMETER (MAX_V0S = 32)
#include "ysvwrk.h"
#include "bcs.h"
C
        DATA NYS0L /0/
        SAVE NYS0L
C
#include "bmacro.h"
C
        IF (NYS0L.EQ.0) THEN
            NYS0L = NAMIND('YS0L')
        ENDIF
C
C+  Check presence of work banks
C
        IF (INDYS0L.LE.0 .OR. INDYS0M.LE.0) THEN
            IER = -1
            GOTO 999
        ENDIF
C
C+  Check validity of input
C
        IF (IROW.GT.IW(INDYS0L+LMHROW)) THEN
            IER = 1
            GOTO 999
        ENDIF
C
C+  Check presence of V0 bank
C
        KYS0L = IW(NYS0L)
        IF (KYS0L.LE.0) THEN
            IER = -1
            GOTO 999
        ENDIF
C
C+  Check validity of input
C
        IF (IV0.GT.MIN(LROWS(KYS0L),MAX_V0S))THEN
            IER = 1
            GOTO 999
        ENDIF
C
C+  Update the work banks
C
C+  Put V0 number into work bank
        IW(KROW(INDYS0L,IROW)+1) = IV0
C+  Store mask for this V0
        IF (IROW.EQ.1) THEN
            IW(KROW(INDYS0M,1)+1) = IBSET(0,MOD(IV0-1,32))
        ELSE
            IW(KROW(INDYS0M,IROW)+1) =
     &         IBSET(IW(KROW(INDYS0M,IROW-1)+1),
     &         MOD(IV0-1,32))
        ENDIF
        IER = 0
 999    CONTINUE
        RETURN
        END
#endif
