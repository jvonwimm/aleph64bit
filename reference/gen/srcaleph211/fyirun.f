      SUBROUTINE FYIRUN(BFIEL,TKDRO,SHDRO,CUTRA)
C ----------------------------------------------------------
CKEY FYXX MCARLO KINE INIT /USER
C J.Hilgart - 871307   B.Bloch 901010
C! Fxxx DST format one-time initialization.
C
C INPUTS:
C       BFIEL       = Magnetic field in kGauss
C       TKDRO       = IF true THEN drop history of interactions
C                     in ITC electronics and TPC endplate
C       SHDRO       = IF true THEN drop history of interactions
C                     in calorimeters
C       CUTRA       = IF not 0. THEN drop track with momentum
C                     below CUTRA
C
C - Called from    USER              from ALEPHLIB.HLB
C - Calls   BKFMT                    from BOS77.HLB
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (LWORDB = 65536)
      PARAMETER (MAXMCX = 1000, MAXMES = 500)
      COMMON /FYRELA/ IHTYPE,ICALTO
     &               ,ITRCAL(MAXMES),ICALTR(2,MAXMES)
     &               ,LTKNUM,LVXNUM,IMEATR,KINTRK,KINVER
     &               ,FSHDRO,FTKDRO,CUTRAC,BFLCLT,ENMINC
     &               ,INDKIN,INDVER,JDKEKS,JDVNFO,JDVOUT
     &               ,JDKOFN,JDKNFO
      LOGICAL FSHDRO,FTKDRO
      PARAMETER (LFXWBK = 7)
      INTEGER  JDFXWB(LFXWBK)
      EQUIVALENCE (JDFXWB(1),INDKIN)
      COMMON /FYCHAR/ ELISAD
      CHARACTER*48 ELISAD
CKEY KINE KINGAL HAC
      PARAMETER(JKEVRN=1,JKEVNT=2,JKEVNV=3,JKEVPI=4,JKEVWT=5,JKEVSR=6,
     +          JKEVTR=7,LKEVHA=7)
      PARAMETER(JKHIHC=1,LKHISA=1)
      PARAMETER(JKJOJD=1,JKJOJT=2,JKJOAV=3,JKJODV=4,JKJODC=5,LKJOBA=5)
      PARAMETER(JKLIGN=1,LKLINA=1)
      PARAMETER(JKPOKI=1,JKPOHX=2,JKPOHY=3,JKPOHZ=4,LKPOLA=4)
      PARAMETER(JKRUGI=1,JKRUNO=2,JKRURT=3,JKRUFS=15,JKRUSS=16,
     +          LKRUNA=16)
      PARAMETER(JKVOVN=1,JKVOVM=2,LKVOLA=2)
      LOGICAL SHDRO,TKDRO
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
C ----------------------------------------------------------------------
C Some constants
      ENMINC = -1.0
      FSHDRO = SHDRO
      FTKDRO = TKDRO
      CUTRAC = CUTRA
      BFLCLT = BFIEL*CLGHT*1.E-5
C
C What the Fxxx adds to the E list: (5 banks)
      ELISAD = 'FKINFVERFPOIFPOLFZFR'
C
C Their formats:
      CALL BKFMT('FKIN','2I,(4F,4I)')
      CALL BKFMT('FVER','2I,(4F,3I,2A)')
      CALL BKFMT('FPOL','2I,(I,3F)')
      CALL BKFMT('FZFR','2I,(F)')
      CALL BKFMT('FPOI','I')
C
      NAJOB = NAMIND('AJOB')
      KAJOB = IW(NAJOB)
      IF (KAJOB .GT. 0) THEN
         ISIML = ITABL(KAJOB,1,2)
         IF (ISIML.EQ.101 .OR. ISIML.EQ.111) THEN
C FAST or SIMDST: add the SIMDST banks to 'E'list
            ELISAD = ELISAD//'FSTRFSCOFLTRFLCOFTOCFTTMFTCM'
            CALL BKFMT('FSCO','2I,(F,I,7F,I)')
            CALL BKFMT('FLTR','I')
            CALL BKFMT('FLCO','I')
            CALL BKFMT('FTOC','I')
            CALL BKFMT('FTTM','I')
            CALL BKFMT('FTCM','I')
         ENDIF
      ENDIF
C
C - get history type from KRUN bank
      IHTYPE = 0
      NKRUN = NAMIND('KRUN')
      JKRUN = IW(NKRUN)
      IF (JKRUN .GT. 0) THEN
         IGCOD = ITABL(JKRUN,JKRUGI,1)
         IHTYPE = MOD (IGCOD,1000) / 100
      ENDIF
C
C Work bank indices one-time initialization
      DO 1 I=1,LFXWBK
  1   JDFXWB(I) = 0
C
 990  CONTINUE
      END
