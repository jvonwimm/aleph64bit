      INTEGER FUNCTION ALREDB (LBASE,LIST)
C -------------------------------------------------------------------
C - F.Ranjard - 870505
C! Read the data base
C - input   : LBASE = data base logical unit (must be .ne. 0)
C             LIST  = list of bank names to be retreived
C - output  : ALREDB= # of banks found (0 if error)
C   ---------------------------------------------------
      CHARACTER*(*) LIST,NAME*4,NLIST*4,CHAINT*4,TKARD*4
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
      LCARD = IW(5)
      IW(5) = LBASE
C
      ALREDB = 0
C
 1    CONTINUE
      KARD = MREADC (IW)
      IF (KARD .NE. 0) THEN
         TKARD = CHAINT (IW(KARD-3))
         I = 0
 2       I = I+1
         NAME = NLIST (IW,I,LIST)
         IF (NAME .NE. ' ') THEN
            IF (TKARD .NE. NAME) GOTO 2
            CALL BLIST (IW,'C+',TKARD)
            ALREDB = ALREDB + 1
            GOTO 1
         ELSE
            CALL BDROP (IW,TKARD)
         ENDIF
         GOTO 1
      ENDIF
C
      IW(5) = LCARD
      END
