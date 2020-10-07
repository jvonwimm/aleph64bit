      SUBROUTINE ITDRFT(INDEX,I1,I3)
C.
C...ITDRFT  3.10  920213  14:18                         R.Beuselinck.
C.
C!  Calculate the drift time due to a set of hits in a single cell.
C.  INDEX contains pointers to the 3rd word (wire no.) of each hit
C.  ordered by increasing wire number.
C.  I1 and I3 give the first and last indices in INDEX for hits in
C.  the current cell.
C.
C.  Called by: ITDIGI                                 from this .HLB
C.      Calls: WBANK                                  from BOS77
C.             RNDM                                   from CERNLIB
C.
C.  Named banks used:
C.  IHIT - read only.
C.  Work banks used:
C.  JDITWP - created at start of each event. Filled on each call.
C.
C-----------------------------------------------------------------------
      SAVE
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
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
      PARAMETER (LIHIT=8, MIHIT=500, LITWP=3)
      PARAMETER (LITFN=6, LITFP=2)
      COMMON /ITNAMC/NAIHIT,NAIDIG,NAITTR,NAIDHR,NAIXBW,
     + JDITWP,JDITFN,JDITFP,JDITNW,JDITAB,JDITDC,JDIZSC
C
      PARAMETER (LITWBK = 7)
      INTEGER JDITWB(LITWBK)
      EQUIVALENCE (JDITWB(1),JDITWP)
C
      COMMON/ITPARC/DVELIT(8,5),HWEFIT
      REAL DVELIT,HWEFIT
C
C
      EXTERNAL INTCHA, RNDM
C
      INTEGER INDEX(*),I1,I3
      REAL XB(4),YB(4)
      LOGICAL OK
      PARAMETER (SMALL = 1.E-6)
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
C
C - Get indices for 'IHIT'
      JIHIT = IW(NAIHIT)
      LNHT  = LROWS(JIHIT)
      LWHT  = LCOLS(JIHIT)
      IGO   = JIHIT + LMHLEN
C
C--  Create temporary wire-pulse bank on first call for each event.
C--
      IF (JDITWP.EQ.0) THEN
        CALL WBANK(IW,JDITWP,LMHLEN+LITWP*LNHT,*998)
        IW(JDITWP+LMHCOL) = LITWP
        IW(JDITWP+LMHROW) = 0
        IW(JDITWP-3) = INTCHA('ITWP')
      ELSE
        IF (LFRWRD(JDITWP) .LT. LITWP*(I3-I1+1)) THEN
          ND = IW(JDITWP) + IW(JDITWP)/2
          CALL WBANK(IW,JDITWP,ND,*998)
        ENDIF
      ENDIF
C
C--  Get parameters of the layer needed for calculating the grid
C--  interpolation.
C--
      II = IGO + INDEX(I1) - 2
      LAY = IW(II+1)
      NW  = IW(II+2)
C
C--  Loop over all hits on the current wire.
C--
      LNWP  = IW(JDITWP+LMHROW)
      DO 100 I=I1,I3
C
C--     Compute random hardware inefficiency.  The inefficiency
C--     decreases with increasing |Cos(Theta)|.
C--
        IF (HWEFIT.GT.0.0) THEN
          TEST = RNDM(DUM1)
          EFF = 1.0 - HWEFIT*(1. - ABS(COS(RW(II+6))))
          IF (TEST.GT.EFF) GO TO 100
        ENDIF
C
        II = IGO + INDEX(I) - 2
        IDHT = INDEX(I)/LWHT + 1
        ITCUR = IW(II)
C
C--     Smear drift distance and compute drift time using
C--     interpolation table.
C--
        DHTMN = RW(II+3)
        DANG  = RW(II+4)
        CALL ITRES(LAY, DHTMN, DANG)
        CALL ITDTIM(LAY, DHTMN, TIME, OK)
        IF (.NOT.OK) GO TO 100
C
C--     Here we have the drift time for one track segment in a cell
C--     (without TOF added).  However, final digitisation comes from the
C--     first signal to arrive from all track segments.  Therefore store
C--     temporarily in induced signals bank and subsequently propagate
C--     to both ends of the wire.
C--     TOF + drift time.
C--
        TPLS = RW(II+7) + ABS(TIME)
C
C--     Check that final time does not come later than TDC stop signal.
        IF (TPLS.GT.TOFFIT) GO TO 100
        JIND = KNEXT(JDITWP)
C--                             ! Hit.ID causing pulse.
        IW(JIND+1) = IDHT
C--                             ! wire no. (redundant but useful).
        IW(JIND+2) = NW
C--                             ! Time of induced wire pulse.
        RW(JIND+3) = TPLS
        IW(JDITWP+LMHROW) = IW(JDITWP+LMHROW) + 1
  100 CONTINUE
      GO TO 9999
C
C - Handle lack of BOS space
 998  CONTINUE
      IW(1) = LITWBK
      CALL WDROP(IW,JDITWB)
      CALL ALTELL('ITDRFT: not enough space for ITxx ',1,'NEXT' )
C
 9999 CONTINUE
      END
