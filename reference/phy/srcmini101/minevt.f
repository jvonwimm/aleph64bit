      SUBROUTINE MINEVT
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill event information bank DEVT for Mini-DST.
C
C     Author: Stephen Haywood      08-Jan-93
C
C     Input  : LOLE, XTCN, VFHL, PECO, PRPW, PEWI, PHST banks
C     Output : DEVT bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
C
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
      PARAMETER(JLOLFB=1,JLOLSP=5,JLOLTO=6,JLOLMA=7,JLOLHV=11,JLOLER=12,
     +          LLOLEA=12)
      PARAMETER(JXTCTT=1,JXTCGC=3,JXTCLL=4,JXTCBN=5,JXTCCL=6,JXTCTR=7,
     +          LXTCNA=16)
      PARAMETER(JPEWMN=1,JPEWPD=2,JPEWSS=47,JPEWTI=55,LPEWIA=55)
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
      PARAMETER(JPRPPN=1,JPRPMR=2,JPRPMN=3,JPRPF1=4,JPRPF2=5,JPRPER=6,
     +          LPRPWA=6)
      PARAMETER(JPHSTI=1,JPHSPI=2,JPHSCE=3,JPHSPH=4,LPHSTA=4)
      PARAMETER(JDEVLE=1,JDEVWT=2,JDEVNX=3,JDEVNV=4,JDEVWR=5,JDEVWA=6,
     +          JDEVWB=7,JDEVPR=8,JDEVPA=9,JDEVPB=10,JDEVLA=11,
     +          JDEVLB=12,JDEVHR=13,JDEVHA=14,JDEVHB=15,LDEVTA=15)
C
      PARAMETER (MXPEC=200)
      DIMENSION EWIRE(36),ERAW(MXPEC)
      SAVE EWMIN
      DATA EWMIN / 0.500 /
      DATA COSHC / 0.700 /
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
C++   Create the DEVT bank.
C
      LEN = LMHLEN + LDEVTA
      CALL AUBOS('DEVT',0,LEN, KDEVT,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINEVT: Cannot create DEVT bank'')')
         RETURN
      ENDIF
      IW(KDEVT+LMHCOL) = LDEVTA
      IW(KDEVT+LMHROW) = 1
C
C++   Store LOLE error flag (used by XLUMOK).
C
      KLOLE = NLINK('LOLE',0)
      IF (KLOLE.GT.0) IW(KROW(KDEVT,1)+JDEVLE) = ITABL(KLOLE,1,JLOLER)
C
C++   Ecal wire t0 (energy weighted over modules).
C
      CALL VZERO(EWIRE,36)
      KPEWI = NLINK('PEWI',0)
      IF ( KPEWI.EQ.0) KPEWI = NLINK('PWEI',0)
      IF (KPEWI.GT.0) THEN
         NPEWI = LROWS(KPEWI)
         SUME = 0.
         SUMT = 0.
         DO 20 I=1,NPEWI
            KEV = 0
            DO 10 IPLAN=1,45
   10       KEV = KEV + ITABL(KPEWI,I,JPEWPD-1+IPLAN)
            EGEV = FLOAT(KEV) / 1000000.
            EWIRE(ITABL(KPEWI,I,JPEWMN)) = EGEV
            IF (EGEV.LT.EWMIN) GOTO 20
            IMODU = ITABL(KPEWI,I,JPEWMN)
            TIME = EMTZER( IMODU)
            SUME = SUME + EGEV
            SUMT = SUMT + EGEV * TIME
   20    CONTINUE
         TIME = 0.
         IF (SUME.GT.0.) TIME = SUMT / SUME
         IW(KROW(KDEVT,1)+JDEVWT) = NINT(TIME)
      ENDIF
C
C++   Store number of GBX counted since last trigger - can help in
C++   identifying Vdet noise.
C
      KXTCN = IW(NAMIND('XTCN'))
      IF (KXTCN.GT.0) IW(KROW(KDEVT,1)+JDEVNX) = ITABL(KXTCN,1,JXTCGC)
C
C++   Store number of Vdet hits - can help in identifying Vdet noise.
C
      NHIT = 0
      KVFHL = IW(NAMIND('VFHL'))
   30 IF (KVFHL.NE.0) THEN
         NHIT = NHIT + LROWS(KVFHL)
         KVFHL = IW(KVFHL-1)
         GOTO 30
      ENDIF
      IW(KROW(KDEVT,1)+JDEVNV) = NHIT
C
C++   Sum raw energies from Ecal wires (PEWI).
C++   'A' <-> endcap A; 'B' <-> endcap B; 'R' <-> barrel.
C
      PEWIA = VSUM(EWIRE( 1),12)
      PEWIR = VSUM(EWIRE(13),12)
      PEWIB = VSUM(EWIRE(25),12)
      IW(KROW(KDEVT,1)+JDEVWR) = NINT(EFACTM * PEWIR)
      IW(KROW(KDEVT,1)+JDEVWA) = NINT(EFACTM * PEWIA)
      IW(KROW(KDEVT,1)+JDEVWB) = NINT(EFACTM * PEWIB)
C
C++   Sum corrected energies from Ecal pads (PECO).
C++   Use PRPW to find fractions in different modules for each PECO.
C++   (Note raw energy in PECO and PRPW not same due to gas gain
C++   corrections applied only to PECO.)
C++   Do not consider Lcal or Sical here.
C
      KPRPW = NLINK('PRPW',0)
      KPECO = NLINK('PECO',0)
      IF (KPRPW.LE.0 .OR. KPECO.LE.0) GOTO 59
      NPRPW = LROWS(KPRPW)
C
      CALL VZERO(ERAW,MXPEC)
      DO 50 I=1,NPRPW
         IPECO = ITABL(KPRPW,I,JPRPPN)
         IF (IPECO.GT.MXPEC) GOTO 50
         ERAW(IPECO) = ERAW(IPECO) + RTABL(KPRPW,I,JPRPER)
   50 CONTINUE
C
      PECOR = 0.
      PECOA = 0.
      PECOB = 0.
      DO 55 I=1,NPRPW
         IPECO = ITABL(KPRPW,I,JPRPPN)
         KODE = ITABL(KPECO,IPECO,JPECKD)
         IF (KODE.EQ.192 .OR. KODE.EQ.256) GOTO 55
         IF (IPECO.LE.MXPEC) THEN
            FRACT = RTABL(KPRPW,I,JPRPER) / ERAW(IPECO)
         ELSE
            FRACT = RTABL(KPRPW,I,JPRPER) / RTABL(KPECO,IPECO,JPECER)
         ENDIF
         ECOR = FRACT * RTABL(KPECO,IPECO,JPECEC)
         MODUL = ITABL(KPRPW,I,JPRPMN)
         IF (MODUL.LE.12) THEN
            PECOA = PECOA + ECOR
         ELSE IF (MODUL.GE.25) THEN
            PECOB = PECOB + ECOR
         ELSE
            PECOR = PECOR + ECOR
         ENDIF
   55 CONTINUE
C
      IW(KROW(KDEVT,1)+JDEVPR) = NINT(EFACTM * PECOR)
      IW(KROW(KDEVT,1)+JDEVPA) = NINT(EFACTM * PECOA)
      IW(KROW(KDEVT,1)+JDEVPB) = NINT(EFACTM * PECOB)
   59 CONTINUE
C
C++   Sum corrected energies from Luminosity monitors (PECO).
C
      PECOA = 0.
      PECOB = 0.
      IF (KPECO.LE.0) GOTO 69
      DO 65 I=1,LROWS(KPECO)
         KODE = ITABL(KPECO,I,JPECKD)
         IF (KODE.NE.192 .AND. KODE.NE.256) GOTO 65
         ECOR = RTABL(KPECO,I,JPECEC)
         THETA = RTABL(KPECO,I,JPECTH)
         IF (THETA.LT.PIBY2) THEN
            PECOA = PECOA + ECOR
         ELSE
            PECOB = PECOB + ECOR
         ENDIF
   65 CONTINUE
C
      IW(KROW(KDEVT,1)+JDEVLA) = NINT(EFACTM * PECOA)
      IW(KROW(KDEVT,1)+JDEVLB) = NINT(EFACTM * PECOB)
   69 CONTINUE
C
C++   Sum corrected energies from Hcal pads (PHCO).
C++   Since the towers are summed online, the distinction between
C++   barrel and encaps must be made on the basis of angle and is less
C++   well defined than for the Ecal.
C
C     KPHCO = NLINK('PHCO',0)
C     IF (KPHCO.GT.0) THEN
C        NPHCO = LROWS(KPHCO)
C        PHCOR = 0.
C        PHCOA = 0.
C        PHCOB = 0.
C        DO 70 I=1,NPHCO
C           COST = COS(RTABL(KPHCO,I,JPHCTH))
C           ECOR = RTABL(KPHCO,I,JPHCEC)
C           IF (COST.GT. COSHC) THEN
C              PHCOA = PHCOA + ECOR
C           ELSE IF (COST.LT.-COSHC) THEN
C              PHCOB = PHCOB + ECOR
C           ELSE
C              PHCOR = PHCOR + ECOR
C           ENDIF
C  70    CONTINUE
C        IW(KROW(KDEVT,1)+JDEVHR) = NINT(EFACTM * PHCOR)
C        IW(KROW(KDEVT,1)+JDEVHA) = NINT(EFACTM * PHCOA)
C        IW(KROW(KDEVT,1)+JDEVHB) = NINT(EFACTM * PHCOB)
C     ENDIF
C
C++   Sum corrected energies from Hcal storeys (PHST).
C
      KPHST = NLINK('PHST',0)
      IF (KPHST.GT.0) THEN
         NPHST = LROWS(KPHST)
         PHSTR = 0.
         PHSTA = 0.
         PHSTB = 0.
         DO 70 I=1,NPHST
            ITHET = MOD(ITABL(KPHST,I,JPHSTI),128)
            ECOR = RTABL(KPHST,I,JPHSCE)
            IF (ITHET.LE.13) THEN
               PHSTA = PHSTA + ECOR
            ELSE IF (ITHET.GE.50) THEN
               PHSTB = PHSTB + ECOR
            ELSE
               PHSTR = PHSTR + ECOR
            ENDIF
   70    CONTINUE
         IW(KROW(KDEVT,1)+JDEVHR) = NINT(EFACTM * PHSTR)
         IW(KROW(KDEVT,1)+JDEVHA) = NINT(EFACTM * PHSTA)
         IW(KROW(KDEVT,1)+JDEVHB) = NINT(EFACTM * PHSTB)
      ENDIF
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DEVT')
C
      RETURN
      END
