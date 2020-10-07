      INTEGER FUNCTION KMPART (IPART,WIDTH,IANTI)
C -----------------------------------------------------------
C - F.Ranjard - 890203
C
C! Complete the PART bank with the width and the antiparticle#
CKEY KINE KINGAL PART FORMAT  /  USER  INTERNAL
C  first Get the necessary name-indices and check that the # of
C  columns of the PART bank is .ge. 10
C  then  IF the particle exists THEN
C           fill word(9) and word(10) of the given particle
C        ELSE
C           RETURN
C        END IF
C  then  Return the particle row # or 0
C
C - structure : INTEGER FUNCTION subprogram
C               User Entry Name: KBPART
C               External References: NAMIND(BOS77)
C               Comdeck References: BCS, BMACRO, PARTJJ
C
C - usage : IRETU  = KMPART (IPART,WIDTH,IANTI)
C - input : IPART  = Aleph particle# (row# in PART bank).
C           WIDTH  = Mass width (or 0.)
C           IANTI  = row# of the antiparticle
C - output: KMPART = Aleph particle#
C                    0 means no PART bank
C                   -1       PART bank has not enough columns
C                   -2       Aleph particle does not exist
C                   -3       antiparticle does not exist
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
CKEY KINE KINGAL DEFAULT
      PARAMETER (LHKIN=3, LPKIN=5, LKVX=2, LHVER=3, LPVER=5, LVKI=50)
      PARAMETER (LGDCA=32)
      PARAMETER (LRPART=200, LCKLIN=1)
      PARAMETER (LRECL=16020, LRUN=1, LEXP=1001, LRTYP=1000)
      CHARACTER*60 LTITL
      PARAMETER (LUCOD=0, LNOTRK=100, LTITL='KINGAL run')
      PARAMETER (LUTRK=350)
      PARAMETER (BFIEL=15., CFIEL=BFIEL*3.E-4)
      PARAMETER(JPARGN=1,JPARNA=2,JPARCO=5,JPARMA=6,JPARCH=7,JPARLT=8,
     +          JPARMW=9,JPARAN=10,LPARTA=10)
      EXTERNAL NAMIND
      DATA NAPAR /0/
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
C ---------------------------------------------------------
      KMPART = 0
C
C - The 1st time get PART bank and check the # of columns
C
      IF (NAPAR .EQ.0 ) THEN
         NAPAR = NAMIND ('PART')
         JPART = IW(NAPAR)
         IF (JPART.EQ.0) THEN
            KMPART = 0
            GOTO 999
         ELSEIF (LCOLS(JPART).LT.JPARAN) THEN
            KMPART = -1
            GOTO 999
         ENDIF
      ENDIF
C
C - Get PART bank, return if does not exist or if part# or antipart#
C   does not exist
      JPART = IW(NAPAR)
      IF (JPART.EQ.0) GOTO 999
      NPART = IW(JPART+LMHROW)
      IF (IPART .GT. NPART) THEN
         KMPART = -2
         GOTO 999
      ELSEIF (IANTI .GT. NPART) THEN
         KMPART = -3
         GOTO 999
      ENDIF
C
C - Normal entry
      KPART = KROW(JPART,IPART)
      RW(KPART+JPARMW) = WIDTH
      IW(KPART+JPARAN) = IANTI
C
      KMPART = IPART
C
 999  CONTINUE
      END
