      INTEGER FUNCTION GTSTUP (DET,IRUN)
C -----------------------------------------------------------
C - F.Ranjard - 901018
CKEY ALEF ADBR DETECTOR SETUP
C! get setup code for a given detector
C
C - Input  : DET      / A = CHARACTER*2 variable to define the
C                           detector: 'BP VD IT TP EC LC SA HC MU TR GE'
C            IRUN     / I = run number
C - Output : GTSTUP   / I = setup code
C                           =-1 in case of error
C - Description :
C      IF run < 2001 THEN
C       montecarlo : get period number from ASIM
C       IF ASIM does not exist THEN get period number from run date
C      ELSE run>= 2001 THEN
C       raw data : get period number from run number
C      ENDIF
C
C -------------------------------------------------------------
      CHARACTER*2 DET
      INTEGER ADBRPN,ADBRUN
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (JASIYM=1,LASIMA=1)
      PARAMETER(JRUNEN=1,JRUNRN=2,JRUNRT=3,JRUNSD=4,JRUNST=5,LRUNHA=5)
      CHARACTER*28 DLIST
      DATA DLIST / 'BPVDITTPECLCPMHCMUTRGEDBSIBE'/
      DATA NASIM / 0/ , NRUNH/0/
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
C ------------------------------------------------------------
C - 1st entry
C
      IF (NASIM.EQ.0) THEN
         NASIM = NAMIND ('ASIM')
         NRUNH = NAMIND ('RUNH')
      ENDIF
C
C - next entry
C
      IPERIOD = 0
      IF (IRUN.LT.2001) THEN
C      monte carlo input file
         JASIM = IW(NASIM)
         IF (JASIM .GT. 0) THEN
            IPERIOD = ITABL(JASIM,1,JASIYM)
         ELSE
            JRUNH = IW(NRUNH)
            IF (JRUNH.GT.0) IPERIOD = IW(JRUNH+JRUNSD)/100
         ENDIF
         NPERIOD = ADBRPN (DET,IROW,IPERIOD,NRUN,ISETUP)
         IF (NPERIOD.EQ.0) ISETUP = -1
      ELSE
C      raw data
         NRUN = ADBRUN (DET,IROW,IPERIOD,IRUN,ISETUP)
         IF (NRUN.EQ.0) ISETUP = -1
      ENDIF
C
      GTSTUP  = ISETUP
C
      END
