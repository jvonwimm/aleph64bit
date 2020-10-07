      SUBROUTINE ITWINI
C.
C...ITWINI  2.30  920213  19:31                         R.Beuselinck
C.
C!  Initialise layout of ITC and time expansion constants for readout.
C.
C.  Called by: ITIRUN                                    from this .HLB
C.      Calls: ISBGIN                                    from this .HLB
C.             BLIST, NAMIND                             from BOS77
C.             ALTELL                                    from ALEPHLIB
C.
C-----------------------------------------------------------------------
      SAVE
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
      COMMON /ITELEC/TDCRIT,TOFFIT,TFINIT(8),ITTDCL(8),ITTDCH(8),
     +               TDCZIT,ITZTZB,ZEXPIT,ZPARIT(3),ITZTDL(8),
     +               ITZTDH(8),ZLOFIT(8),ZRESIT(2,8)
C
      COMMON/ITEXPC/CLOKIT,CLGOIT,TSTRIT,TBINIT,PLENIT(8),EXPFIT(8)
C
      COMMON/ITROTC/EULRIT(3),DXYZIT(3),ROTITC(3,3),ITSHFT,WSAGIT
      REAL EULRIT,DXYZIT,ROTITC,WSAGIT
      LOGICAL ITSHFT
C
      COMMON/ITWIRC/RWIRIT(8),NWIRIT(8),IWIRIT(8),PHWRIT(8),CELHIT(8),
     +              CELWIT(8),WZMXIT
C
      EXTERNAL NAMIND
      PARAMETER(JIALID=1,JIALVR=2,JIALDX=4,JIALDR=7,JIALIS=10,LIALIA=10)
      PARAMETER(JIDRID=1,JIDRVR=2,JIDRTO=4,JIDRCO=5,LIDRPA=9)
      PARAMETER(JIGEID=1,JIGEVR=2,JIGENA=4,JIGERI=8,JIGERO=9,JIGEZN=10,
     +          JIGEZX=11,JIGEIP=12,LIGEOA=12)
      PARAMETER(JILYID=1,JILYVR=2,JILYLN=4,JILYNW=5,JILYRW=6,JILYPO=7,
     +          JILYCH=8,JILYIP=9,LILYRA=9)
      PARAMETER(JINOID=1,JINOVR=2,JINOIT=4,LINOIA=4)
      PARAMETER(JIPMID=1,JIPMVR=2,JIPMHN=4,JIPMIS=5,LIPMOA=5)
      PARAMETER(JIRFID=1,JIRFVR=2,JIRFBW=4,JIRFZB=5,JIRFDV=6,JIRFMC=7,
     +          JIRFCL=15,JIRFCH=23,JIRFIT=31,LIRFEA=31)
      PARAMETER(JIRRID=1,JIRRVR=2,JIRRMX=4,JIRRMN=5,JIRRCP=6,JIRRCN=11,
     +          LIRRFA=15)
      PARAMETER(JISCID=1,JISCVR=2,JISCSN=4,JISCIT=5,LISCOA=5)
      PARAMETER(JISLID=1,JISLVR=2,JISLXS=4,JISLRS=7,JISLIS=10,LISLOA=10)
      PARAMETER(JISUID=1,JISUVR=2,JISUIP=4,LISURA=4)
      PARAMETER(JITCID=1,JITCVR=2,JITCCN=4,JITCNL=5,JITCWL=6,JITCWS=7,
     +          LITCCA=7)
      PARAMETER(JITKID=1,JITKVR=2,JITKSR=4,JITKSZ=5,JITKRS=6,JITKRL=7,
     +          JITKIT=8,LITKPA=8)
      PARAMETER(JITRID=1,JITRVR=2,JITRIT=4,LITRGA=4)
      PARAMETER(JIZNLO=1,JIZNS1=9,JIZNS2=10,JIZNS3=11,LIZNLA=11)
      PARAMETER(JIZFID=1,JIZFVR=2,JIZFBW=4,JIZFZB=5,JIZFEX=6,JIZFS1=7,
     +          JIZFS2=8,JIZFS3=9,JIZFOO=10,JIZFCL=11,JIZFCH=19,
     +          JIZFIT=27,LIZFEA=27)
      PARAMETER(JIZRID=1,JIZRVR=2,JIZRLN=4,JIZRCO=5,JIZRTR=7,LIZRSA=7)
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
C--  Get ITC readout geometry and other constants from the databse.
C--
      IND = IW(NAMIND('ITCC')) + LMHLEN
      NLAY   = IW(IND+JITCNL)
      WZMXIT = RW(IND+JITCWL)
      WSAGIT = RW(IND+JITCWS)
      ITSHFT = WSAGIT.NE.0.
C
C--  Fill the sense wire layout data.
C--
      IND = IW(NAMIND('ILYR'))
      NR  = LROWS(IND)
      IF (NR.NE.NLAY)
     +  CALL ALTELL('ITWINI: Wrong number of layers in ILYR',0,'STOP')
      DO 30 I=1,NR
        JROW = KROW(IND,I)
        LAY = IW(JROW+JILYLN)
        NWIRIT(LAY) = IW(JROW+JILYNW)
        RWIRIT(LAY) = RW(JROW+JILYRW)
        PHWRIT(LAY) = PI*IW(JROW+JILYPO)/NWIRIT(LAY)
        CELHIT(LAY) = RW(JROW+JILYCH)
        CELWIT(LAY) = TWOPI*RWIRIT(LAY)/NWIRIT(LAY)
   30 CONTINUE
      NOFF = 0
      DO 35 I=1,NR
        IWIRIT(I) = NOFF
        NOFF = NOFF + NWIRIT(I)
   35 CONTINUE
C
C--  Fill the Z-scalar data with fixed numbers until the obsolete
C--  IZSC bank is replaced.  These numbers are only used by the
C--  SPP trigger (which doesn't exist yet).
C--
      PLEN   = 55.
      CLOKIT = 20.
      TSTRIT = 732.
      TBINIT = 6.
      DO 40 I=1,NLAY
        PLENIT(I) = 0.5*PLEN * RWIRIT(NLAY)/RWIRIT(I)
   40 CONTINUE
C
C--  Fill /ITELEC/ with the R-phi & Z TDC parameters.
C--
      IND = IW(NAMIND('IRFE')) + LMHLEN
      TDCRIT = RW(IND+JIRFBW)
      TOFFIT = TDCRIT*IW(IND+JIRFZB)
      DO 50 I=1,NLAY
        ITTDCL(I) = IW(IND+JIRFCL+I-1)
        ITTDCH(I) = IW(IND+JIRFCH+I-1)
   50 CONTINUE
      IND = IW(NAMIND('IZFE')) + LMHLEN
      TDCZIT = RW(IND+JIZFBW)
      ITZTZB = IW(IND+JIZFZB)
      ZEXPIT = RW(IND+JIZFEX)
      IND2 = IW(NAMIND('IZNL')) + LMHLEN
      IF (IW(IND+JIZFOO).EQ.0) THEN
        ZPARIT(1) = RW(IND2+JIZNS1)
        ZPARIT(2) = RW(IND2+JIZNS2)
        ZPARIT(3) = RW(IND2+JIZNS3)
      ELSE
        ZPARIT(1) = 0.
        ZPARIT(2) = 0.
        ZPARIT(3) = 1000.
      ENDIF
      DO 60 I=1,NLAY
        ZLOFIT(I) = RW(IND2+JIZNLO+I-1)
        ITZTDL(I) = IW(IND+JIZFCL+I-1)
        ITZTDH(I) = IW(IND+JIZFCH+I-1)
        EXPFIT(I) = ZEXPIT * RWIRIT(NLAY)/RWIRIT(I)
   60 CONTINUE
C
C--  Get the Z resolution parameters for each layer.
C--
      IND = IW(NAMIND('IZRS'))
      DO 70 I=1,LROWS(IND)
        JROW = KROW(IND,I)
        LAY           = IW(JROW+JIZRLN)
        ZRESIT(1,LAY) = RW(JROW+JIZRCO)
        ZRESIT(2,LAY) = RW(JROW+JIZRCO+1)
   70 CONTINUE
C
C--  Initialise the S-bend interpolation.
C--
      CALL ISBGIN(WZMXIT, ZPARIT(1), ZPARIT(2), ZPARIT(3))
      END
