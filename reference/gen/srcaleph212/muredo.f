      SUBROUTINE MUREDO(IER)
C----------------------------------------------------------------------
C
CKEY MUONID MUON RERUN / INTERNAL
C
C!
C!   Authors   :- G.Taylor
C!
C!   This routine can be called to rerun the muon identification part
C!   of JULIA outside the context of JULIA. Must call MUNEWR every time
C!   a new run is encountered.
C!
C!           IER /I = Error flag
C!                  = 0 ok
C!                  = 2 bad run for hcal digital pattern
C!                  > 10  problem in hrhtub
C!                  > 100 problem in mrmhit
C!                  > 1000 problem in muido
C!
C!======================================================================
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      LOGICAL HDODGY
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
C----------------------------------------------------------------------
      IER=0
      CALL ABRUEV (KRUN,KEVE)
      IF (HDODGY(KRUN)) THEN
C
C This run is bad for muons. Set error flag.
C
        IER = 2
      ENDIF

      CALL BDROP(IW,'HROAMUEXHMADMTHRMHITMCADMUIDTREXD4CD')

      IF (KRUN.LT.2000) THEN
        CALL HRHTUB(IERT)
        IF (IERT.NE.0) IER = IER+10*IERT
      ENDIF
C
      CALL FTRACK
      CALL HMFIND
      IF (KRUN.LT.2000) THEN
        CALL MPREDM
      ELSE
        CALL MPREDG
      ENDIF
      IF (KRUN.LT.2000) THEN
        CALL MRMHIT(IERT)
        IF (IERT.NE.0) IER = IER+100*IERT
      ENDIF
      CALL MUASS
      CALL MUIDO(IERT)
      IF (IERT.NE.0) IER = IER+1000*IERT
      CALL BLIST(IW,'E+','MUIDD4CD')

      RETURN
      END
