      SUBROUTINE BKTOBK (BANK1,NR1,BANK2,NR2)
C --------------------------------------------------------
CKEY ALEF BANK COPY
C - F.RANJARD - 950518
C! copy bank1,nr1 into bank2,nr2
C - Input  : BANK1  /A = input bank name
C            NR1    /I = input bank number
C            BANK2  /A = output bank name
C            NR2    /I = output bank number
C --------------------------------------------------------
      CHARACTER*(*) BANK1, BANK2
      INTEGER NR1, NR2
      CHARACTER*196 FMT
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C --------------------------------------------------------
C
      JBANK1 = NLINK(BANK1,NR1)
C     return if BANK1,NR1 does not exist
      IF (JBANK1.EQ.0) GOTO 999
C
C - create BANK2,NR2 with BANK1 length
C
      CALL AUBOS (BANK2,NR2,IW(JBANK1),JBANK2,IGARB)
C     return if not enough space
      IF (JBANK2.EQ.0) GOTO 999
      IF (IGARB.EQ.1) JBANK1 = NLINK(BANK1,NR1)
C
C - copy BANK1 into BANK2
C
      CALL UCOPY (IW(JBANK1+1),IW(JBANK2+1),IW(JBANK1))
C
C - set BANK2 format if necessary
C
      NBANK2 = NAMIND(BANK2)
C     if BANK2 format already set then return
      IF (IGTFMT(NBANK2).GT.0) GOTO 999
C
      NBANK1 = NAMIND(BANK1)
      ID1    = IGTFMT(NBANK1)
C     if BANK1 format not set then return
      IF (IGTFMT(NBANK1).EQ.0) GOTO 999
C
C     unpack BANK1 format, set BANK2 format
      CALL TRFMTR (FMT,LFM,IW(ID1+1),IW(ID1))
      CALL BKFMT (BANK2,FMT(1:LFM))
C
 999  CONTINUE
      END
