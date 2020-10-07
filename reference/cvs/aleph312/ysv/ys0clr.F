        SUBROUTINE YS0CLR
C----------------------------------------------------------------------------
C!  Drop V0 work banks
C
C       Author:  D.Casper   04/07/96
C
C----------------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "ysvwrk.h"
C
#include "bmacro.h"
C
C Drop workbanks INDYS0L, INDYS0M, INDHXNU, and INDVHNU
        IW(1) = 4
        CALL WDROP(IW,INDYS0L)

        RETURN
        END
#endif
