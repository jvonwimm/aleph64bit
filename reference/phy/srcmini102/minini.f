      SUBROUTINE MININI(NWANT,WLIST)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Mini code initialisation.
C
C     Author: Stephen Haywood      06-Jan-93
C
C     Output : NWANT  = number of banks listed on MINI card
C                     = 1 if no MINI card
C              WLIST  = list of banks on MINI card
C                     = ' ' if no MINI card
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
      CHARACTER*800 WLIST
      CHARACTER*4 CHAINT,BANK
      INTEGER ALFMT
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
C++   Print the version number.
C
      WRITE(6,'(//40X,''**** Mini-DST version'',I5,'' ****'')')
     &  MINVSN(DUMMY)
C
C++   Identify which of the standard Mini banks are wanted from MINI
C++   card. Default is all.
C++   Note: EVEH and DVRS are forced to be copied to o/p.
C++   Note: NWANT=1 is necessary even if WLIST is empty because of
C++   statement function in MINBLD.
C
      WLIST = ' '
      KMINI = NLINK('MINI',0)
      NMINI = IW(KMINI)
      IF (NMINI.GT.0) THEN
         WLIST(1:4) = 'EVEH'
         WLIST(5:8) = 'DVRS'
         NWANT = 2
         DO I=1,NMINI
            IF (NWANT.LT.MAXCHA/4) THEN
               BANK = CHAINT(IW(KMINI+I))
               IF (BANK.NE.'EVEH' .AND. BANK.NE.'DVRS') THEN
                  NWANT = NWANT + 1
                  WLIST(4*NWANT-3:4*NWANT) = CHAINT(IW(KMINI+I))
               ENDIF
            ENDIF
         ENDDO
      ELSE
         NWANT = 1
      ENDIF
C
C++   Define run list.
C
      CALL MINRUN
C
C++   Define bank formats.
C
      IER = ALFMT(LUNIT,'ALL ',FORM)
C
C
      RETURN
      END
