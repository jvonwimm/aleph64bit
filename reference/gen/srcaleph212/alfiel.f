      FUNCTION ALFIEL(DUM)
C --------------------------------------------------------------
C - F.Ranjard - 890803       modified - 900419
C - modified by: D.Colling - 930111
C                take 92 current offset into account
C                F.Ranjard - 931213
C                the 92 current offset is valid for 92 and 93
C                F.Ranjard - 940624
C                polarity was wrong for run 25261 and 25265
C                give the correct mag.field value
C! Returns the magnetic field
CKEY FIELD
C - Input    : DUM             = dummy argument
C - Output   : ALFIEL  / R     = magnetic field value
C
C - IF (FIEL data card exists) THEN
C      ALFIEL = FIEL word(1)
C   ELSE IF montecarlo (AFID exists) THEN
C      ALFIEL = AFID word(JAFIMF)
C   ELSE IF raw data (RALE exists) THEN
C      ALFIEL = RALE word(13)*word(14) * nominal field/current
C   ELSE no bank
C      ALFIEL = 15. (nominal field)
C   ENDIF
C   IF (no FIEL and  RUNH or RUNR) THEN
C      IF (run number > 6000) ALFIEL = -ALFIEL
C   ENDIF
C ------------------------------------------------------------------
      SAVE NFIEL, NRALE, NRUNH, NRUNR
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C     FIENOM = nominal magnetic field
C     CURNOM = current giving a nominal magnetic field
C     ICOCUR = compesation coil current
      PARAMETER (FIENOM=15., CURNOM=4963750., ICOCUR=800000)
C     CUR92C = current offset for 92,93
      PARAMETER (CUR92C=17700.)
C
      PARAMETER(JRUNEN=1,JRUNRN=2,JRUNRT=3,JRUNSD=4,JRUNST=5,LRUNHA=5)
      PARAMETER(JAFIAR=1,JAFIAZ=2,JAFIMF=3,JAFIBE=4,LAFIDA=4)
      PARAMETER(JRALRN=1,JRALLR=2,JRALRS=3,JRALRT=4,JRALTT=6,JRALTM=8,
     +          JRALEP=9,JRALMP=13,JRALMC=14,JRALMA=15,JRALMB=16,
     +          JRALNA=17,JRALNB=18,JRALQA=19,JRALQB=20,JRALDU=21,
     +          JRALCO=22,JRALVD=24,JRALIT=26,JRALTP=27,JRALEC=29,
     +          JRALHC=32,JRALMU=35,JRALSA=41,JRALLC=43,JRALBC=45,
     +          LRALEA=46)
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
C - set name-indices
      IF (NAFID .EQ. 0) THEN
         NAFID = NAMIND ('AFID')
         NFIEL = NAMIND ('FIEL')
         NRALE = NAMIND ('RALE')
         NRUNH = NAMIND ('RUNH')
         NRUNR = NAMIND ('RUNR')
      ENDIF
C
C - get the mag. field
C
      IF (IW(NFIEL) .NE. 0) THEN
C        data card FIEL
         FIELD = RW(IW(NFIEL)+1)
C
      ELSEIF (IW(NAFID) .NE. 0) THEN
C        montecarlo run
         FIELD = RTABL (IW(NAFID),1,JAFIMF)
C
      ELSEIF (IW(NRALE) .NE. 0) THEN
C        raw data
C
C        get the run number
         IF (IW(NRUNH).GT.0) THEN
            JRUN = IW(NRUNH)
         ELSEIF(IW(NRUNR).GT.0) THEN
            JRUN = IW(NRUNR)
         ELSE
            GOTO 999
         ENDIF
         IRUN = IW(JRUN+JRUNRN)
C
         POLA  = ITABL (IW(NRALE),1,JRALMP)
         CURR  = ITABL (IW(NRALE),1,JRALMC)
         ICA   = ITABL (IW(NRALE),1,JRALMA)
         ICB   = ITABL (IW(NRALE),1,JRALMB)
C
C        1.1% correction if compensation coils are off
         CCOR  = 1.
         IF (ICA.LT.ICOCUR .OR. ICB.LT.ICOCUR) CCOR = 1.011
C
C        correct for 92 offset 14000 < run < 25000
         IF (IRUN.GT.14000 .AND. IRUN.LT.25000) THEN
           CURR = CURR - CUR92C
         ENDIF
C
         FIELD = POLA*CURR*CCOR*FIENOM/CURNOM
C
C        change the sign if run number > 6000
         IF (IRUN .GT. 6000) FIELD = -FIELD
C
C        give the correct mag.field value
         IF (IRUN .EQ.11961) FIELD = 15.011
         IF (IRUN .EQ.25261 .OR. IRUN.EQ.25265) FIELD = 15.021
C
      ELSE
C        no bank
         FIELD = FIENOM
      ENDIF
C
 999  CONTINUE
      ALFIEL = FIELD
C
      END
