        SUBROUTINE YSFSET(NVX,NTK,NV0,NCIR,IER)
C----------------------------------------------------------------------------
C!   Create work banks for secondary vertex fit
C
C       Author:  D.Casper 04/07/96
C
C       NVX, NTK, NV0, NCIR:  Number of vertices, tracks, V0s and circles
C       IER:    Return code from individual set-up routines
C-----------------------------------------------------------------------------
#ifndef DOC
        CALL YSFCLR
        CALL YSVSET(NVX,IER)
        IF (IER.NE.0) RETURN
        CALL YSTSET(NTK,IER)
        IF (IER.NE.0) RETURN
        CALL YS0SET(NV0,IER)
        IF (IER.NE.0) RETURN
        CALL YSCSET(NCIR,IER)
        RETURN
        END
#endif
