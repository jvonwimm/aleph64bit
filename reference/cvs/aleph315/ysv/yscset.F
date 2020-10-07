      SUBROUTINE YSCSET(NCIR,IER)
C------------------------------------------------------------------------------
C!  Create workbank to hold ITC tracks to be fit
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
        CALL WBANK(IW,INDYSCL,LMHLEN+NCIR,*999)
        IW(INDYSCL-3) = INTCHA('YSCL')
        IW(INDYSCL+LMHCOL) = 1
        IW(INDYSCL+LMHROW) = NCIR

        CALL WBANK(IW,INDYSCM,LMHLEN+4*NCIR,*999)
        IW(INDYSCM-3) = INTCHA('YSCM')
        IW(INDYSCM+LMHCOL) = 4
        IW(INDYSCM+LMHROW) = NCIR

        CALL WBANK(IW,INDCXOU,LMHLEN+3*NCIR,*999)
        IW(INDCXOU-3) = INTCHA('CXOU')
        IW(INDCXOU+LMHCOL) = 3
        IW(INDCXOU+LMHROW) = NCIR
 
        CALL WBANK(IW,INDVCOU,LMHLEN+6*NCIR,*999)
        IW(INDVCOU-3) = INTCHA('VCOU')
        IW(INDVCOU+LMHCOL) = 6
        IW(INDVCOU+LMHROW) = NCIR

        IER = 0
 999    CONTINUE
        RETURN
        END

#endif
