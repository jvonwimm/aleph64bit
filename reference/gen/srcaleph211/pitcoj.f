      SUBROUTINE PITCOJ( LIST , IER )
C----------------------------------------------------------------------
CKEY PTOJ UNPACK ITCO
C!  Author   :- D. SCHLATTER             10-APR-1989
C!  Modified :- J. Sedgbeer  90/01/04  Mod. for IFCO bank
C!  Modified :- J. Sedgbeer  90/04/20  Mod. to create ICCO bank
C!  Modified :- J. Sedgbeer  92/01/30  Remove obsolete IFCO code
C!
C! Get ITCO and ICCO banks from POT banks. 1. unpack digitizings
C!                                         2. calculate coordinates
C!                                         3. make corrected coords.
C!  Inputs:     LIST  /C    BOS event list
C!       -                  if LIST(2:2).eq.'-' drop POT banks
C!
C!  Outputs:    IER = 0  successful unpacking of all banks.
C!                  = 1  some missing or empty input banks
C!                  = 2  No room to create some/all banks
C!                  = -1 O.K. but garbage collection done.
C!
C!  Called by :   AUNPCK
C!  Calls     :   IPTOJ,ICRCOO,IMICCO
C?
C!======================================================================
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      CHARACTER*(*) LIST*(*)
      EXTERNAL NAMIND
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        NAITCO=NAMIND('ITCO')
      ENDIF
C
C Unpack digitizings  (PIDI -> IDIG + FICL)
      CALL IPTOJ ( LIST, IER )
      IF(IER.GT.0) GOTO 999
C
C Determine coordinates again  (IDIG -> ITCO + IDCR)
      CALL ICRCOO(IER)
      IF(IER.GE.3) IER = 1
      IF(IER.GT.0) GOTO 999
      CALL BLIST (IW,'S+','ITCO')
C
C Make corrected coords bank  (ITCO + FRFT -> ICCO)
      CALL IMICCO(IER)
      IF(IER.GT.0) GOTO 999
      CALL BLIST(IW,'S+','ICCO')
C
  999 RETURN
      END
