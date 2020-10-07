      SUBROUTINE ITDIGI
C.
C...ITDIGI  1.07  890417  14:53                        R.Beuselinck.
C.
C!  Create the digitisings for the ITC.
C.
C.  Simulate the drift time distribution of the cells and find the
C.  drift times corresponding to the accumulated hits.
C.  Propagate the induced signals to the electronics and generate the
C.  digitisings.
C.
C.  Called by: ASDIGI                                   from this .HLB
C.      Calls: WBANK, WDROP, BKFRW, BLIST               from BOS77
C.             ITTOFE, ITXTLK, ITAMP, ITDAQ             from this .HLB
C.             SORTZV, LOCATI                           from CERNLIB
C.
C.  Work banks used:
C.  JDITWP  - dropped at end.
C.
C-----------------------------------------------------------------------
      SAVE
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
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
      COMMON/ITSUMC/NEVHIT,NEVDIT,NTCUIT,NHCUIT,NDCUIT,DIGHIT,
     +      NTSMIT(10),NHSMIT(10),NDSMIT(10),NDHSIT(10)
      INTEGER NEVHIT,NEVDIT,NTCUIT,NHCUIT,NDCUIT,
     +      NTSMIT,NHSMIT,NDSMIT,NDHSIT
      REAL DIGHIT
C
C
      INTEGER INDEX(1000)
       COMMON /WRKSPC/ WSPACE(88320)
      EQUIVALENCE (WSPACE(1),INDEX(1))
      EXTERNAL LOCATI, LOCATF
      INTEGER IBINT(11),IBINH(11),IBIND(11)
      REAL BINDH(11)
      DATA IBINT/0,10,20,30,40,50,60,70,80,90,9999/
      DATA IBINH/0,50,100,150,200,250,300,400,600,800,9999/
      DATA IBIND/0,50,100,150,200,250,300,400,600,800,9999/
      DATA BINDH/0.,.2,.4,.6,.7,.75,.8,.85,.9,.95,1./
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
C -----------------------------------------------------------
C
C--  Drop output banks if they exist.
C--
      CALL BDROP(IW,'IDIGIDHRITTRIXRP')
      JDITWP = 0
      JDITFN = 0
      JDITFP = 0
C
CD    WRITE(LOUTIO,1000)
C
      JIHIT = IW(NAIHIT)
      IF (JIHIT.EQ.0) GOTO 999
      LW    = LCOLS(JIHIT)
      LHIT  = LROWS(JIHIT)
      IF (LHIT.EQ.0) GOTO 999
C
C--  Sort all the hits in order of wire number. (3rd column).
C--
      DO 50 I=1,LHIT
        INDEX(I) = 3+(I-1)*LW
   50 CONTINUE
      KIHIT = JIHIT + LMHLEN
      CALL SORTZV(IW(KIHIT+1),INDEX,LHIT,-1,0,1)
CD    WRITE(LOUTIO,1001) (IW(KIHIT+INDEX(I)),I=1,LHIT)
C
C--  Loop over each hit wire and calculate the drift time.
C--
      I1 = 1
   80 IF (I1.LE.LHIT) THEN
        I2 = I1
        I3 = I1
        NW1 = IW(KIHIT+INDEX(I1))
   90   IF (I2.LE.LHIT) THEN
          IF (NW1.EQ.IW(KIHIT+INDEX(I2))) THEN
            I3 = I2
            I2 = I2 + 1
            GO TO 90
          ENDIF
        ENDIF
        CALL ITDRFT(INDEX,I1,I3)
        I1 = I3 + 1
        GO TO 80
      ENDIF
C
C--  ITWP bank. Reduce the size of the bank first.
C--
      ND = LMHLEN + LCOLS(JDITWP)*LROWS(JDITWP)
      CALL WBANK(IW,JDITWP,ND,*999)
C
C--  Propagate signals to ITC preamps at the ends of the chamber.
C--
      CALL ITTOFE
C
C--  Generate cross-talk on adjacent channels
C--
      CALL ITXTLK
C
C--  Amplify final front-end signals.
C--
      CALL ITAMP
C
C--  Simulate the readout chain. Create digitisings and trigger signals.
C--
      CALL ITDAQ
C
C--  Print out of digitising bank.
C--
      IF (FDEBJO .AND. IPRIJO(2).EQ.1) CALL ITPRDI
C
C--  Complete summary statistics for current event.
C--
      NEVDIT = NEVDIT + 1
      NHCUIT = LROWS (JIHIT)
      DIGHIT = FLOAT(NDCUIT)/FLOAT(NHCUIT)
      IF (NTCUIT .GT. 0) THEN
         IBIN = ABS(LOCATI(IBINT,11,NTCUIT))
         NTSMIT(IBIN) = NTSMIT(IBIN) + 1
      ENDIF
      IBIN = ABS(LOCATI(IBINH,11,NHCUIT))
      NHSMIT(IBIN) = NHSMIT(IBIN) + 1
      IBIN = ABS(LOCATI(IBIND,11,NDCUIT))
      NDSMIT(IBIN) = NDSMIT(IBIN) + 1
      IBIN = MIN(ABS(LOCATF(BINDH,11,DIGHIT)),10)
      NDHSIT(IBIN) = NDHSIT(IBIN) + 1
C
  999 CONTINUE
      CALL WDROP(IW,JDITWP)
      CALL WDROP(IW,JDITFN)
      CALL WDROP(IW,JDITFP)
C
 1000 FORMAT(/' ++++ITDIGI++++  ITC digitisation called.')
 1001 FORMAT(/' Ordered wire numbers from IHIT bank.',
     +  /(1X,30I4))
      END
