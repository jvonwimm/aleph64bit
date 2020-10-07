        SUBROUTINE YSFCLR
C----------------------------------------------------------------------------
C!   Drop work banks for secondary vertex fit
C
C       Author:  D.Casper 04/07/96
C
C-----------------------------------------------------------------------------
#ifndef DOC
        CALL YS0CLR
        CALL YSTCLR
        CALL YSVCLR
        CALL YSCCLR
        RETURN
        END
#endif
