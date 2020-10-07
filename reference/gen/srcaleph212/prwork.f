      SUBROUTINE PRWORK(IWORK)
C-----------------------------------------------------------------------
C! Print tabular work bank
C!
C  Author  S. Orteu       22-JUN-1987
C
C Can be used to print named banks, but the information on formats
C won't be used. So it's suitable to print named banks whose
C format has not been defined
C
C - structure : SUBROUTINE subprogram
C               User Entry Names: PRWORK
C               External References: BPRWRK, CHAINT
C               Common decks       : BCS, BMACRO
C
C - usage   : CALL PRWORK (INDEX)
C - input   : INPUT : index of work bank
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER*4 NAME,CHAINT
      EXTERNAL CHAINT
      LOGICAL FTABL
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
      FTABL(K) =LCOLS(K).LT.256 .AND. LROWS(K).LT.50000
C-----------------------------------------------------------------------
      IF (IWORK.EQ.0. OR. IW(IWORK) .EQ. 0) THEN
         WRITE(IW(6),*) 'Bank doesn''t exist'
         GOTO 999
      ENDIF
      NCOL = LCOLS(IWORK)
      NROW = LROWS(IWORK)
      NDATA = NCOL*NROW
      NAME = CHAINT(IW(IWORK-3))
      LUNP=IW(6)
      IF (FTABL(IWORK)) THEN
          WRITE(IW(6),1000) NAME,NCOL,NROW,IW(IWORK)
          CALL BPRWRK(IW(IWORK+3),1,NDATA,LUNP,NCOL)
       ELSE
          WRITE(IW(6),*) 'Non-tabular bank. It will be skipped'
       END IF
 999  RETURN
 1000 FORMAT(/1X,'+++PRWORK+++ Bank ',A4,' has ',I3,
     &  ' columns and ',I6,' rows and the total length is ',I6)
      END
