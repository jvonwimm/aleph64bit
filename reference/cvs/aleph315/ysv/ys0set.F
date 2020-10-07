      SUBROUTINE YS0SET(NV0,IER)
C------------------------------------------------------------------------------
C!  Create workbank to hold V0's to be fit
C
C       Author:  D.Casper 04/07/96
C       
C           IER = 1 Unable to create workbank
C           IER = 0 Normal
C------------------------------------------------------------------------------
#ifndef DOC
#include "ysvwrk.h"
#include "bcs.h"
C
#include "bmacro.h"
C
        IER = 1
        CALL WBANK(IW,INDYS0L,LMHLEN+NV0,*999)
        IW(INDYS0L-3) = INTCHA('YS0L')
        IW(INDYS0L+LMHCOL) = 1
        IW(INDYS0L+LMHROW) = NV0

        CALL WBANK(IW,INDYS0M,LMHLEN+NV0,*999)
        IW(INDYS0M-3) = INTCHA('YS0M')
        IW(INDYS0M+LMHCOL) = 1
        IW(INDYS0M+LMHROW) = NV0

        CALL WBANK(IW,INDHXNU,LMHLEN+5*NV0,*999)
        IW(INDHXNU-3) = INTCHA('HXNU')
        IW(INDHXNU+LMHCOL) = 5
        IW(INDHXNU+LMHROW) = NV0

        CALL WBANK(IW,INDVHNU,LMHLEN+15*NV0,*999)
        IW(INDVHNU-3) = INTCHA('VHNU')
        IW(INDVHNU+LMHCOL) = 15
        IW(INDVHNU+LMHROW) = NV0

        IER = 0
 999    CONTINUE
        RETURN
        END

#endif
