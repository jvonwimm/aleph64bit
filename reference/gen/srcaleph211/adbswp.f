      SUBROUTINE ADBSWP(IOLD,INEW)
C --------------------------------------------------------------------
CKEY ALEF DBASE DCARD SWAP / USER
C! swap data base bank given on data cards with NR=iold to NR=inew
C - F.Ranjard - 900626
C
C - Input argument    : IOLD   / INTE = data card bank number
C                       INEW   / INTE = swap to new bank number
C -------------------------------------------------------------------
      INTEGER ALGTD1,USGTD1
      CHARACTER*4 NAME,CHAINT
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C ------------------------------------------------------------------
C
      LUN = JUNIDB(0)
C
C - loop over existing named banks
C
      IND  = IW(14)+3
 10   IF (IND .GE. IW(15)) GOTO 20
      LE   = IW(IND)
      IF (LE.LT.0) THEN
C        bank has been dropped
         IND = IND - LE
      ELSE
C        bank is there, is it a data base bank?
         NAME = CHAINT (IW(IND-3))
         CALL BDADIR (LUN,NAME,IPOS)
         IF (IPOS.GT.0) THEN
            NR = IW(IND-2)
            IF (NR.EQ.IOLD) THEN
               NEW = NSWAP (NAME,IOLD,NAME,INEW)
            ENDIF
         ENDIF
         IND = IND + LE + 4
      ENDIF
      GOTO 10
 20   CONTINUE
C
C - set data card bank number of ALGTDB to INEW
C
      IER = ALGTD1 (INEW)
      IER = USGTD1 (INEW)
C
      END
