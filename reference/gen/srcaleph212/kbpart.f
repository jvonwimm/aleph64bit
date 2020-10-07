      INTEGER FUNCTION KBPART (IGEA,NAME,ITRTP,ZMAS,ZCHA,TLIF)
C -----------------------------------------------------------
C - J.Boucrot - B.Bloch - F.Ranjard - 870424
C
C! Complete the PART bank with new particles
CKEY KINE KINGAL FILL BANK /   INTERNAL
C  first Get the necessary name-indices and the NOtracking marker word
C        from the KRUN bank
C  then  IF it is a new particle  THEN
C           fill a new row of the PART bank
C        ELSE IF it is a particle already known by GEANT THEN
C           modify the content of the corresponding row
C        END IF
C  then  Return the particle row #
C
C - structure : INTEGER FUNCTION subprogram
C               User Entry Name: KBPART
C               External References: AUBOS/LOCTAB(ALEPHLIB), NAMIND(BOS7
C               Comdeck References: BCS, KIPARA, BMACRO
C
C - usage : IPART  = KBPART (IGEA,NAME,ITRTP,ZMAS,ZCHA,TLIF)
C - input : IGEA   = Geant#
C           NAME   = particle name (at most 12 char.)
C           ITRTP  = Geant tracking type
C           ZMAS   = particle mass
C           ZCHA   = particle charge
C           TLIF   = particle time life (if TLIF>1000. then TLIF=1.E+15)
C - output: KBPART = Aleph particle#
C                    0 means no PART bank
C                    - N means : PART is full with N particles
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
      EXTERNAL NAMIND
      CHARACTER*(*) NAME
      PARAMETER (TLMAX=1000., TSTAB=1.E+15)
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
      NUPART(JBOS,NGEAN) = LOCTAB (IW(JPART+LMHLEN+1),LCOLS(JPART),
     &                             LROWS(JPART),1,NGEAN)
C ---------------------------------------------------------
      KBPART = 0
C
C - The 1st time get NOtracking marker word
C
      IF (NAPAR .EQ.0 ) THEN
         NAPAR = NAMIND ('PART')
         JKRUN = IW(NAMIND ('KRUN'))
         IF (JKRUN.NE.0) THEN
            NOTRK = ITABL(JKRUN,1,2)
         ELSE
            NOTRK = 0
         ENDIF
      ENDIF
C
C - Get PART bank, return if does not exist
      JPART = IW(NAPAR)
      IF (JPART.EQ.0) GOTO 999
      LPART = IW(JPART+LMHCOL)
      NPART = IW(JPART+LMHROW)
C
C - Normal entry
C   is it a particle already known: check if the Geant#
C   is .NE. NOTRK and already there
C
      NUP = 0
      IF (IGEA .NE. NOTRK) NUP = NUPART(JPART,IGEA)
      IF (NUP.GT.0) THEN
C     the particle is already there, update the content
         KPART = KROW (JPART,NUP)
      ELSE
C     Add the new particle at the end of the PART bank
         IF (LFRROW(JPART).LT.1) THEN
            CALL AUBOS ('PART',0,IW(JPART)+LPART*LRPART,JPART,IGARB)
            IF (JPART.EQ.0) THEN
               KBPART = -NPART
               GOTO 999
            ENDIF
         ENDIF
         NPART = NPART + 1
         KPART = KNEXT(JPART)
      ENDIF
C
C - Fill the row# NPART at the index KPART
      IW(KPART+1) = IGEA
      DO 101 L=1,3
         LL = 4*(L-1)+1
         IW(KPART+1+L) = INTCHA (NAME(LL:LL+3))
 101  CONTINUE
      IW(KPART+5) = ITRTP
      RW(KPART+6) = ZMAS
      RW(KPART+7) = ZCHA
      IF (TLIF .GT. TLMAX) TLIF = TSTAB
      RW(KPART+8) = TLIF
C
      IW(JPART+LMHROW) = NPART
C
      KBPART = NPART
C
 999  CONTINUE
      END
