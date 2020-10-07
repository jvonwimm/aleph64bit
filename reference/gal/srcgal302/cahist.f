      SUBROUTINE CAHIST(NAMI,IAD,IST,ISIG)
C--------------------------------------------------------------------
C     M.N MINARD 14-11-88
C! Calorimeter history banks
C
C  INPUT : NAME = NAME-INDEX OF BANK TO BE FILLED
C          IAD  = ADDRESS
C          IST  = STACK OR PLANE NUMBER
C          ISIG = SIGNAL
C  OUTPUT: History Banks ESHI EWHI / LSHI LWHI / HSHI
C
C  Called by EHSITW, LCTRAK, LCSHOW
C--------------------------------------------------------------------
      SAVE
      EXTERNAL LOCTAB
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      PARAMETER(JESHPT=1,JESHTI=2,JESHDE=3,LESHIA=5)
      PARAMETER(JEWHPT=1,JEWHMI=2,JEWHDE=3,LEWHIA=47)
      PARAMETER(JLSHPT=1,JLSHTI=2,JLSHDE=3,LLSHIA=5)
      PARAMETER(JLWHPT=1,JLWHMI=2,JLWHDE=3,LLWHIA=40)
      PARAMETER(JHSHPT=1,JHSHSI=2,JHSHDE=3,LHSHIA=4)
      PARAMETER (JHISPT=1, JHISAD=2, JHISDE=3)
      CHARACTER *4 TNAME , CHAINT
      DATA IFI /0/
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
C-------------------------------------------------------------------
C
C
C - get bank index
C
      JHIST = IW(NAMI)
      IF (JHIST.EQ.0) RETURN
C
C     Look if this track already in bank
C
      ITRK  = ITRKEL(1)
      LHIST = LCOLS(JHIST)
      NWRD  = LHIST - JHISDE + 1
      NHIST = LROWS(JHIST)
C
      IF (NHIST .EQ. 0 ) GOTO 100
C
      IRTRK = 0
      KHIST = KROW(JHIST,NHIST)
      DO 10 IROW = NHIST,1,-1
         IF ( IW (KHIST+JHISPT) .EQ. ITRK) THEN
            IF (IRTRK.EQ.0) IRTRK = IROW
            IF (IW (KHIST+JHISAD) .NE. IAD) GOTO 10
            IW(KHIST+JHISDE+IST-1) = IW(KHIST+JHISDE+IST-1) +ISIG
            GOTO 999
         ELSE
            IF (IRTRK .EQ. 0) GOTO 10
            KHIST = KROW (JHIST,IRTRK+1)
            CALL UCOPY2 (IW(KHIST+1),IW(KHIST+LHIST+1),
     &                   (NHIST-IRTRK)*LHIST)
            DO 5 K = 3,LHIST
  5         IW(KHIST+K) = 0
            GOTO 200
         ENDIF
 10   KHIST = KHIST - LHIST
C
C - add new row
C
 100  KHIST = KNEXT (JHIST)
C
C - store a row
C
 200  IW (KHIST + JHISPT ) = ITRK
      IW (KHIST + JHISAD ) = IAD
      IW (KHIST + JHISDE + IST -1) = ISIG
      IW (JHIST + LMHROW ) = LROWS(JHIST) + 1
C
C - check bank length for next entry
C
       IF (LFRROW(JHIST).EQ.0) THEN
          TNAME = CHAINT (IW(JHIST-3))
          CALL ALBOS (TNAME,0,IW(JHIST)+IW(JHIST)/2,JHIST,IGARB)
       ENDIF
C
 999   CONTINUE
       END
