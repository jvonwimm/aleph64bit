      SUBROUTINE ITTOFE
C.
C...ITTOFE  2.00  920213  19:21                          R.Beuselinck
C.
C!  Propagate induced wire signals to the preamps at either end of ITC.
C.
C.  Input:  IHIT, JDITWP banks.
C.  Output: JDITFN, JDITFP banks.
C.
C.  JDITFN contains: Hit.Id, Wire.Id, End (+/-), pulse start time,
C.                 number of pulses in ITFP,
C.                 zero address of 1st pulse in ITFP.
C.
C.  JDITFP contains: pulse height, pulse length.
C.
C.  This version produces fixed length pulses only of unique height.
C.  The JDITFP bank allows for an arbitrary pulse shape to be recorded
C.  as a continuous series of rectangular pulses.
C.
C.  Called by: ITDIGI, ISBEND                           from this .HLB
C.      Calls: WBANK, WDROP, BLIST, BKFRW               from BOS77
C.
C.  Named banks used:
C.  IHIT   - read only.
C.  JDITWP - read only.
C.  JDITFN - created and filled
C.  JDITFP - created and filled
C.
C-----------------------------------------------------------------------
      SAVE
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
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /ITELEC/TDCRIT,TOFFIT,TFINIT(8),ITTDCL(8),ITTDCH(8),
     +               TDCZIT,ITZTZB,ZEXPIT,ZPARIT(3),ITZTDL(8),
     +               ITZTDH(8),ZLOFIT(8),ZRESIT(2,8)
C
      COMMON/ITWIRC/RWIRIT(8),NWIRIT(8),IWIRIT(8),PHWRIT(8),CELHIT(8),
     +              CELWIT(8),WZMXIT
C
      PARAMETER (LIHIT=8, MIHIT=500, LITWP=3)
      PARAMETER (LITFN=6, LITFP=2)
      COMMON /ITNAMC/NAIHIT,NAIDIG,NAITTR,NAIDHR,NAIXBW,
     + JDITWP,JDITFN,JDITFP,JDITNW,JDITAB,JDITDC,JDIZSC
C
      PARAMETER (LITWBK = 7)
      INTEGER JDITWB(LITWBK)
      EQUIVALENCE (JDITWB(1),JDITWP)
C
      EXTERNAL INTCHA
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
C ------------------------------------------------------------------
C
C--  Get induced signals and propagate them to the end of the chamber.
C--
      IF (JDITWP.EQ.0) GO TO 998
      LWWP = IW(JDITWP+1)
      LNWP = IW(JDITWP+2)
C
      KIHIT = IW(NAIHIT)
      IF (KIHIT.EQ.0) GO TO 998
      LWHT = LCOLS(KIHIT)
      LNHT = LROWS(KIHIT)
C--  Create banks JDITFN and JDITFP.
C--
      CALL WBANK (IW,JDITFN,LMHLEN+LITFN*LNWP*2,*998)
      IW(JDITFN+LMHCOL) = LITFN
      IW(JDITFN+LMHROW) = 0
      IW(JDITFN-3) = INTCHA ('ITFN')
      CALL WBANK (IW,JDITFP,LMHLEN+LITFP*LNWP*2,*998)
      IW(JDITFP+LMHCOL) = LITFP
      IW(JDITFP+LMHROW) = 0
      IW(JDITFP-3) = INTCHA ('ITFP')
C
      JWP  = JDITWP + LMHLEN
      JHT  = KIHIT + LMHLEN
      LWFN = LCOLS (JDITFN)
      LWFP = LCOLS (JDITFP)
      LNFN = 0
      LNFP = 0
C
C--  Loop over all induced signals in the ITWP bank.
C--
      DO 100 I=1,LNWP
        IDHT = IW(JWP+1)
        NW   = IW(JWP+2)
        TPLS = RW(JWP+3)
C
C--     Find hit position on wire and compute timing offset for
C--     signal propagation to the ends of the wire after smearing the
C--     true Z position by the resolution parameterization.
C--
        ZPLS = RTABL(KIHIT,IDHT,6)
        LAY  = ITABL(KIHIT,IDHT,2)
        DZ = ZRESIT(1,LAY) + ZRESIT(2,LAY)*ZPLS**2
        CALL RANNOR(ZERR,WASTE)
        ZERR = ZERR*DZ
        ZPLS = ZPLS + ZERR
        IF (ABS(ZPLS).GT.WZMXIT) ZPLS = SIGN (WZMXIT,ZPLS)
C
C--     Note that the global timing offset is computed according to
C--     a signal at Z=0.  Therefore the correction required is
C--     relative to this point.
C--     Compute measured Z according to S-bend relation. Adjust by
C--     the individual layer offset. Finally convert to a time-diff.
C--
        CALL ISBEND(ZPLS, ZMEAS)
        ZMEAS = ZMEAS - ZLOFIT(LAY)
        DT = 2.0*ZMEAS/CLGHT
C
C--     Assume that the deviation from linear is shared symmetrically
C--     between the signals at +/- Z.
C--
        DPOS = ZPLS/CLGHT - 0.5*DT
        DNEG = - DPOS
        ZPOS = - ZPLS
        ZNEG = + ZPLS
        TPOS = ZPOS/CLGHT + TPLS + DPOS
        TNEG = ZNEG/CLGHT + TPLS + DNEG
        JFN = KNEXT (JDITFN)
        JFP = KNEXT (JDITFP)
C--     zero offset for start of pulse data.
        IPADR = JFP - JDITFP
C
C--     Fill (+/-) front end signal data.
C--
        IW(JFN+1) = IDHT
        IW(JFN+2) = NW
        IW(JFN+3) = +1
        RW(JFN+4) = TPOS
C--     number of pulses stored at front end.
        IW(JFN+5) = 1
        IW(JFN+6) = IPADR
        RW(JFP+1) = 1.
        RW(JFP+2) = 1.
        CALL UCOPY(IW(JFN+1),IW(JFN+LWFN+1),LWFN)
        IW(JFN+LWFN+3) = -1
        RW(JFN+LWFN+4) = TNEG
        IW(JFN+LWFN+6) = IPADR + IW(JFN+5)*LWFP
        IPADR = IW(JFN+LWFN+6)
        RW(JDITFP+IPADR+1) = 1.
        RW(JDITFP+IPADR+2) = 1.
        LNFN = LNFN + 2
        LNFP = LNFP + IW(JFN+5) + IW(JFN+LWFN+5)
        IW(JDITFN+2) = LNFN
        IW(JDITFP+2) = LNFP
        JWP = JWP + LWWP
  100 CONTINUE
C
  999 RETURN
C - not enough space for JDITFN or JDITFP banks
  998 CONTINUE
      IW(1) = LITWBK
      CALL WDROP (IW,JDITWB)
      CALL ALTELL ('ITTOFE: not enough space for ITFx banks ',1,'NEXT')
      END
