      FUNCTION ALELEP(IRUN)
C --------------------------------------------------------------
C - F.Ranjard - 900703
C! Returns the LEP energy for a given run
CKEY ALEF LEP ENERGY
C - Input    : IRUN    / I     = run number
C - Output   : ALELEP  / R     = LEP energy
C
C - IF (a row exists for this run in LFIL) THEN
C      ALELEP = LFIL(run,JLFILE)
C   ELSE IF montecarlo (AFID exists) THEN
C      ALELEP = AFID word(JAFIBE)
C   ELSE IF raw data (RLEP exists) THEN
C      ALELEP = RLEP word(JRLELE)*2./1000.
C   ELSE no bank
C      ALELEP = 91.2
C   ENDIF
C   IF ALELEP<50. THEN ALELEP=91.2
C ------------------------------------------------------------------
      SAVE NLFIL, NRLEP, ELEPMN, ELEPDF
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JAFIAR=1,JAFIAZ=2,JAFIMF=3,JAFIBE=4,LAFIDA=4)
      PARAMETER(JRLELE=1,JRLELB=2,JRLELD=3,JRLELF=4,JRLELP=5,LRLEPA=5)
      PARAMETER(JLFILF=1,JLFIFR=2,JLFILR=3,JLFINV=4,JLFILE=5,JLFIBX=6,
     +          JLFIBY=7,JLFIBZ=8,JLFIEX=9,JLFIEY=10,JLFIEZ=11,
     +          JLFIOF=12,LLFILA=12)
      DATA NAFID /0/
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
C --------------------------------------------------------------------
C
C - set name-indices
      IF (NAFID .EQ. 0) THEN
         NAFID = NAMIND ('AFID')
         NLFIL = NAMIND ('LFIL')
         NRLEP = NAMIND ('RLEP')
         ELEPDF= 91.2
         ELEPMN= 50.
      ENDIF
C
C - get  the LEP energy
C
      IF (IW(NAFID) .NE. 0) THEN
C     montecarlo run
         IF (IW(NRLEP) .NE.0) THEN
            ELEP = REAL(ITABL(IW(NRLEP),1,JRLELE)) / 500.
         ELSE
            ELEP = RTABL (IW(NAFID),1,JAFIBE)
         ENDIF
      ELSE
C
C     not a montecarlo run
C     get LFIL row number for this run from data base
         JLFIL = LFILRU (IRUN,IROW)
         IF (JLFIL.EQ.0 .OR. IROW.LE.0) THEN
C        LFIL does not exist or the run does not exist
C        get RLEP from the SOR record
            IF (IW(NRLEP) .NE. 0) THEN
               ELEP = REAL(ITABL(IW(NRLEP),1,JRLELE)) / 500.
            ELSE
C           noway to find ELEP get the default value
               ELEP = ELEPDF
            ENDIF
         ELSE
C        the run exists in LFIL
            ELEP = RTABL(IABS(JLFIL),IROW,JLFILE)
         ENDIF
      ENDIF
      IF (ELEP.LT.ELEPMN) ELEP = ELEPDF
C
      ALELEP = ELEP
C
      END
