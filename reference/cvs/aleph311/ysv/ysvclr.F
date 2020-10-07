        SUBROUTINE YSVCLR
C----------------------------------------------------------------------------
C!  Drop vertex work banks
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
C  Drop INDYSVW, INDYSEW, INDYSTW, INDYS0W, and INDYSCW
        IW(1) = 5
        CALL WDROP(IW,INDYSVW)

        RETURN
        END
#endif
