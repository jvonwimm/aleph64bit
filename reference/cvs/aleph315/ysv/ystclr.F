        SUBROUTINE YSTCLR
C----------------------------------------------------------------------------
C!  Drop track work banks
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
C  Drop INDYSTL, INDYSTM, INDHXOU, INDVHOU
        IW(1) = 4
        CALL WDROP(IW,INDYSTL)

        RETURN
        END
#endif
