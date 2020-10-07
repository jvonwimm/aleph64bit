      SUBROUTINE YSVSET(NVX,IER)
C------------------------------------------------------------------------------
C!  Create workbank to hold vertices to be fit
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
        CALL WBANK(IW,INDYSVW,LMHLEN+3*NVX,*999)
        IW(INDYSVW-3) = INTCHA('YSVW')
        IW(INDYSVW+LMHCOL) = 3
        IW(INDYSVW+LMHROW) = NVX

        CALL WBANK(IW,INDYSEW,LMHLEN+6*NVX,*999)
        IW(INDYSEW-3) = INTCHA('YSEW')
        IW(INDYSEW+LMHCOL) = 6
        IW(INDYSEW+LMHROW) = NVX

        CALL WBANK(IW,INDYSTW,LMHLEN+4*NVX,*999)
        IW(INDYSTW-3) = INTCHA('YSTW')
        IW(INDYSTW+LMHCOL) = 4
        IW(INDYSTW+LMHROW) = NVX

        CALL WBANK(IW,INDYS0W,LMHLEN+NVX,*999)
        IW(INDYS0W-3) = INTCHA('YS0W')
        IW(INDYS0W+LMHCOL) = 1
        IW(INDYS0W+LMHROW) = NVX

        CALL WBANK(IW,INDYSCW,LMHLEN+NVX,*999)
        IW(INDYSCW-3) = INTCHA('YSCW')
        IW(INDYSCW+LMHCOL) = 1
        IW(INDYSCW+LMHROW) = NVX

        IER = 0
 999    CONTINUE
        RETURN
        END

#endif
