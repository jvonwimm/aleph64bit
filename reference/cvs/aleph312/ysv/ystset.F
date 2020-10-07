      SUBROUTINE YSTSET(NTK,IER)
C------------------------------------------------------------------------------
C!  Create workbank to hold tracks to be fit
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
        CALL WBANK(IW,INDYSTL,LMHLEN+NTK,*999)
        IW(INDYSTL-3) = INTCHA('YSTL')
        IW(INDYSTL+LMHCOL) = 1
        IW(INDYSTL+LMHROW) = NTK

        CALL WBANK(IW,INDYSTM,LMHLEN+4*NTK,*999)
        IW(INDYSTM-3) = INTCHA('YSTM')
        IW(INDYSTM+LMHCOL) = 4
        IW(INDYSTM+LMHROW) = NTK

        CALL WBANK(IW,INDHXOU,LMHLEN+5*NTK,*999)
        IW(INDHXOU-3) = INTCHA('HXOU')
        IW(INDHXOU+LMHCOL) = 5
        IW(INDHXOU+LMHROW) = NTK
 
        CALL WBANK(IW,INDVHOU,LMHLEN+15*NTK,*999)
        IW(INDVHOU-3) = INTCHA('VHOU')
        IW(INDVHOU+LMHCOL) = 15
        IW(INDVHOU+LMHROW) = NTK

        IER = 0
 999    CONTINUE
        RETURN
        END

#endif
