        SUBROUTINE YSCCLR
C----------------------------------------------------------------------------
C!  Drop circle work banks
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
C  Drop workbanks INDYSCL, INDYSCM, INDCXOU, and INDVCOU
        IW(1) = 4
        CALL WDROP(IW,INDYSCL)

        RETURN
        END
#endif
