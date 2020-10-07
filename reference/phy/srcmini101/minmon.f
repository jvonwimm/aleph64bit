      SUBROUTINE MINMON(MODE,IMAQQ, SUMRY)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Simple monitoring of the Mini-DST.
C
C     Author: Stephen Haywood      25-Apr-90
C     Modify: Stephen Haywood      08-Jun-90   v 3.0
C
C     Input  : MODE   = -1 for reset
C                        0 for event monitoring
C                       +1 for termination, then reset
C              IMAQQ  = 1 for hadronic event candidate, 0 otherwise
C     Output : SUMRY  = summary array
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (MAXBNK=200,MAXCHA=4*MAXBNK)
      CHARACTER*800 MLISTE,MLISTR
      LOGICAL MINIMC,NOFORM
      COMMON / MINCOM / MLISTE,MLISTR,MINIMC,NOFORM
C
      CHARACTER*4 CHAINT,NAME,SNAME,TAG
      LOGICAL GOTIT
      DIMENSION BANK(MAXBNK),TAG(0:1)
      INTEGER BANK
      DIMENSION ISUM(MAXBNK,0:3,0:1),TOT(-1:+1),INDX(MAXBNK)
      DIMENSION SUMRY(7)
      SAVE TAG,TOT,LBANK,BANK,ISUM
      DATA TAG / ' all','  QQ' /
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
C
      IF(MODE.NE.0) GOTO 1000
C
C++   Increment counters.
C
      TOT(-1) = TOT(-1) + 1.
      IF(IMAQQ.LT.0) RETURN
      IF(IMAQQ.GE.0) TOT(0) = TOT(0) + 1.
      IF(IMAQQ.GE.1) TOT(1) = TOT(1) + 1.
C
C++   Loop over each bank on Mini list.
C
      DO 100 JBANK=1,LENOCC(MLISTE)/4
         NAME = MLISTE(4*JBANK-3:4*JBANK)
C
C++      See if this is a known bank; if not add it to the list.
C
         IBANK = IUCOMP(INTCHA(NAME),BANK,LBANK)
         IF(IBANK.EQ.0) THEN
            IF(LBANK.LT.MAXBNK) THEN
               LBANK = LBANK + 1
               BANK(LBANK) = INTCHA(NAME)
               IBANK = LBANK
            ELSE
               GOTO 100
            ENDIF
         ENDIF
C
C++      Pick up link to bank (may be compressed).
C
         GOTIT = .FALSE.
   50    KBANK = NAMIND(NAME) + 1
   60    KBANK = IW(KBANK-1)
         IF(KBANK.LE.0) THEN
            SNAME = NAME
            CALL CUTOL(NAME(1:1))
            IF (NAME.NE.SNAME) THEN
               GOTO 50
            ELSE
               GOTO 100
            ENDIF
         ENDIF
C
C++      Find the bank's size.
C
         NROWS = LROWS(KBANK)
         NCOLS = LCOLS(KBANK)
         LENTH = IW(KBANK)
         DO 110 I=0,IMAQQ
            IF(.NOT.GOTIT) ISUM(IBANK,0,I) = ISUM(IBANK,0,I) + 1
            ISUM(IBANK,1,I) = ISUM(IBANK,1,I) + NROWS
            ISUM(IBANK,2,I) = ISUM(IBANK,2,I) + 4 + 2 + NCOLS*NROWS
            ISUM(IBANK,3,I) = ISUM(IBANK,3,I) + 4 + LENTH
  110    CONTINUE
         GOTIT = .TRUE.
         GOTO 60
  100 CONTINUE
C
      RETURN
C
C++   Termination.
C
 1000 IF(MODE.LT.0) GOTO 2000
C
C++   Small modification for EVEH which is not tabular.
C
      IEVEH = IUCOMP('EVEH',BANK,LBANK)
      IF(IEVEH.GT.0) THEN
         DO 1010 I=0,1
            ISUM(IEVEH,1,I) = 0.
            ISUM(IEVEH,2,I) = ISUM(IEVEH,3,I)
 1010    CONTINUE
      ENDIF
C
C++   Sort the bank list alphabetically.
C
      CALL SORTZV(BANK,INDX,LBANK,0,0,0)
C
C++   Make some small changes for printing for MC events.
C
      ILAST = 1
      IF (MINIMC) THEN
         TAG(0) = '  MC'
         ILAST = 0
      ENDIF
C
C++   Print statistics.
C
      CALL VZERO(SUMRY,6)
      SUMRY(1) = TOT(-1)
      DO 1100 I=0,ILAST
         IF(TOT(I).LE.0.) GOTO 1100
         WRITE(6,'(/'' Statistics for'',A4,'' events :'')') TAG(I)
         WRITE(6,
     &     '(''      Bank      Frac      Rows     Bytes     Bytes'')')
         SUMU = 0.
         SUMC = 0.
         DO 1200 JBANK=1,LBANK
            IBANK = INDX(JBANK)
            FRAC = FLOAT(ISUM(IBANK,0,I)) / TOT(I)
            ROWS = FLOAT(ISUM(IBANK,1,I)) / TOT(I)
            BYTU = FLOAT(ISUM(IBANK,2,I)) / TOT(I) * 4.
            BYTC = FLOAT(ISUM(IBANK,3,I)) / TOT(I) * 4.
            WRITE(6,'(A10,F10.3,F10.1,2F10.0)')
     &        CHAINT(BANK(IBANK)),FRAC,ROWS,BYTU,BYTC
            SUMU = SUMU + BYTU
            SUMC = SUMC + BYTC
 1200    CONTINUE
         WRITE(6,'(''     Total'',F10.0,10X,2F10.0)') TOT(I),SUMU,SUMC
         SUMRY(3*I+2) = TOT(I)
         SUMRY(3*I+3) = SUMC / SUMU
         SUMRY(3*I+4) = SUMC
 1100 CONTINUE
C
C++  Reset list of banks and counters.
C
 2000 CONTINUE
      LBANK = 0
      CALL VZERO(TOT(-1),3)
      CALL VZERO(BANK,MAXBNK)
      CALL VZERO(ISUM(1,0,0),MAXBNK*4*2)
C
      RETURN
      END
