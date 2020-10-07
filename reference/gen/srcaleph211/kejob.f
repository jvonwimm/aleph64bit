      SUBROUTINE KEJOB (LWRT,MXEVT)
C ----------------------------------------------------------------
C - F.Ranjard - 870505
C! KINGAL end of job
CKEY KINE KINGAL FILL BANK /  INTERNAL
C   close output file if it exists and print BOS statistics
C
C - structure: SUBROUTINE program
C              User Entry Name: KEJOB
C              External References: ALRUNE/BKENRU/BKENJO(ALEPHLIB)
C                                   NAMIND/BWRITE/BOSTA(BOS77)
C              Comdecks referenced: BCS, BMACRO
C
C - usage   : CALL KEJOB (LWRT,MXEVT)
C - input   : LWRT   = output file logical unit
C             MXEVT  = # of events in the run
      SAVE
      INTEGER ALRUNE
      EXTERNAL NAMIND
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
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
C ---------------------------------------------------------------
C - Create RUNE bank
      JRUNH = IW(NAMIND('RUNH'))
      IEXP = IW (JRUNH + 1)
      IRUN = IW (JRUNH + 2)
      IRTY = IW (JRUNH + 3)
      IRUNE = ALRUNE ( IRUN,IEXP,IRTY,MXEVT )
      CALL BLIST (IW,'C=','RUNE')
      IF (LWRT.NE.0) THEN
C
C Write end of run bank :
         CALL BWRITE (IW,LWRT,'C')
C
C - Close output file
         CALL BWRITE (IW,LWRT,'0')
      ENDIF
C
C - BOS statistics
      CALL BOSTA
C
      END
