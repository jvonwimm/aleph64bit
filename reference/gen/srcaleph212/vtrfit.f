      SUBROUTINE VTRFIT(ISWIM,FIELD,IER,IGARB,NMATC)
C-----------------------------------------------------------------------
C!Do track-VDET clusters association and refit tracks with VDET info.
CKEY VDET TRACK
C
C  Author      : B. Mours     901001
C  modified by : B. Mours     910918
C     adjust error for large pulses or double pulses
C     order track using extrap. error instead of momentum
C  modified by : I. Tomalin   940728
C     Replace calls to MDARD by calls to ALGTDB.
C     (Except for 1993 data/MC for backwards compatibility).
C  modified by : F.Ranjard    950314
C     remove test on 1993 ==> remove the bug for all years.
C
C  modified by : A. Bonissent 950714
C     reset NASS to a positive value (no effect for old Vdet)
C
C  input:  ISWIM = 1 Means you want the error matrix to be valid
C                    in the region of the interaction point
C                = 0 Means you want the errors at the first measurement
C          FIELD = field value used by the Kalman fitter
C
C  return: IER   =  error flag
C                   = 0  OK
C                     4  not enough space for work bank  (VTRFIT)
C                     5  not enough space for named bank (VTRFIT)
C                     6  missing some named banks        (VTRFIT)
C                    11  not enough space for named bank (VTFILL)
C                    12                                  (VTFILL)
C                    14                                  (VTFILL)
C
C          IGARB = max( IGARB ) Tell the worse case for garbage
C                  collection (usefull for ALPHA job)
C
C          NMATC = Number of matched W,U (respectively)
C
C  Banks needed:  Track bank (PRFT or FRFT), coordinate banks TPCO,
C                 ITCO (and ICCO) are needed.
C                 The unpacking routines should be called if needed.
C
C  Banks created: VDCO,FVCL,  one more FRFT
C
C----------------------------------------------------------------------
      SAVE
C
      INTEGER JVTXWI,JVTXHF,JVTXUC,JVTXWC,JVTXSU,JVTXSW,
     +          JVTXUW,JVTXXC,JVTXYC,JVTXZC,JVTXPV,
     +          JVTXPU,JVTXPW,JVTXUR,JVTXUT,JVTXUP,
     +          JVTXUD,JVTXUZ,JVTXWR,JVTXWT,JVTXWP,
     +          JVTXWD,JVTXWZ,LVTXTA
      PARAMETER(JVTXWI=1,JVTXHF=2,JVTXUC=3,JVTXWC=4,JVTXSU=5,JVTXSW=6,
     +          JVTXUW=7,JVTXXC=8,JVTXYC=9,JVTXZC=10,JVTXPV=11,
     +          JVTXPU=12,JVTXPW=13,JVTXUR=14,JVTXUT=15,JVTXUP=16,
     +          JVTXUD=17,JVTXUZ=18,JVTXWR=19,JVTXWT=20,JVTXWP=21,
     +          JVTXWD=22,JVTXWZ=23,LVTXTA=23)
      INTEGER JVTCEL,JVTCES,JVTCIU,JVTCOU,JVTCLW,JVTCOW,
     +          JVTCUN,JVTCWN,JVTCMS,JVTCAC,JVTCDF,
     +          JVTCNP,JVTCRH,LVTCEA
      PARAMETER(JVTCEL=1,JVTCES=2,JVTCIU=3,JVTCOU=4,JVTCLW=5,JVTCOW=6,
     +          JVTCUN=7,JVTCWN=8,JVTCMS=9,JVTCAC=10,JVTCDF=11,
     +          JVTCNP=12,JVTCRH=13,LVTCEA=13)
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
      PARAMETER(JVCPXB=1,JVCPNX=2,JVCPZB=3,JVCPNZ=4,JVCPC2=5,LVCPLA=5)
      PARAMETER(JVTRMC=1,JVTRML=2,JVTRFN=3,JVTRSC=4,JVTRCC=5,JVTRCI=6,
     +          JVTRMP=7,JVTRBE=8,JVTRUS=9,JVTRWS=10,LVTRPA=10)
      PARAMETER(JVDXXC=1,JVDXYC=2,JVDXUC=3,JVDXSX=4,JVDXSY=5,JVDXSU=6,
     +          JVDXPH=7,JVDXQF=8,JVDXNA=9,JVDXIP=10,JVDXIW=11,
     +          JVDXIH=12,LVDXYA=12)
      PARAMETER(JVDZZC=1,JVDZWC=2,JVDZSZ=3,JVDZSW=4,JVDZPH=5,JVDZQF=6,
     +          JVDZNA=7,JVDZIP=8,JVDZIW=9,JVDZIH=10,LVDZTA=10)
      PARAMETER(JVDCWI=1,JVDCR0=2,JVDCPH=3,JVDCZ0=4,JVDCSR=5,JVDCSZ=6,
     +          JVDCQF=7,JVDCTN=8,LVDCOA=8)
      PARAMETER(JFVCIV=1,LFVCLA=1)
      PARAMETER(JVTMNL=1,JVTMNU=2,JVTMNW=3,JVTMC2=4,JVTMIT=5,JVTMFR=6,
     +          JVTMUW=7,JVTMWW=11,JVTMIU=15,JVTMIW=19,JVTMWI=23,
     +          JVTMR0=27,JVTMPH=31,JVTMZ0=35,JVTMUC=39,JVTMWC=43,
     +          JVTMSU=47,JVTMSW=51,JVTMCO=55,LVTMAA=58)
      PARAMETER(JVTSC2=1,JVTSCI=2,LVTSCA=5)
      PARAMETER(JVTUWI=1,JVTUCI=5,JVTUUC=9,JVTUSU=13,JVTURC=17,
     +          JVTUPH=21,JVTURE=25,LVTUCA=28)
      PARAMETER(JVTWWI=1,JVTWCI=5,JVTWWC=9,JVTWSW=13,JVTWZC=17,
     +          JVTWRE=21,LVTWCA=24)
      INTEGER JVDGMD,JVDGVI,JVDGPH,JVDGNA,LVDGCA
      PARAMETER(JVDGMD=1,JVDGVI=2,JVDGPH=3,JVDGNA=4,LVDGCA=4)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /VTKREC/  NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL
     +                ,NARCVD
      INTEGER NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL,NARCVD
      COMMON /VTRPAR/ MAXCLS,MAXCOM,IVFRFT,C2PRCL,SEACUT,CI2CUT,
     +                BIGERR,PULMIN,USNOIS,WSNOIS,HBIGER,NLYRMX
     +    ,           ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB
      INTEGER MAXCOM,MAXCLS,IVFRFT,NLYRMX
      REAL C2PRCL,SEACUT,CI2CUT,BIGERR,PULMIN,HBIGER
      REAL ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB
C
      DIMENSION HP(7),COV(28)
      LOGICAL FIRST
      INTEGER GTSTUP,IVSTP,IPVSTP,IRUN,IPRUN,LDBAS,JRUNR
      INTEGER NFRTL,NFRFT,NFTCL,NFICL,NICCO,NTPCO,NVTMA,NVTXT
     +        NVDCO,NVTRP,NVDXY,NVDZT,NRUNR
      INTEGER KFRTL,KFRFT,KFTCL,KTPCO,KITCO,KFICL,KVDXY,KVTMA,
     +        KVDZT,LFRFT,LFRTL,JFRFT,JFRTL
      INTEGER IGARB,NTRK,ICL,I,ITK,JTK,NCLST,NTPC,NITC,NVDET,
     +        NDOF,IERR,IOPT,NMULT,IOITC,IOTPC,IOVPT,JVTRP
      INTEGER NMATC(2)
C!    local common to store work bank indices
      INTEGER KWSRT,KVTUC,KVTWC,KVTS0,KVTS1
      COMMON /VTBOS/ KWSRT, KVTUC, KVTWC, KVTS0, KVTS1
C
      INTEGER ALGTDB
      DATA FIRST/.TRUE./
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
C - 1st entry
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NFRTL = NAMIND('FRTL')
        NFRFT = NAMIND('FRFT')
        NVTXT = NAMIND('VTXT')
        NFTCL = NAMIND('FTCL')
        NFICL = NAMIND('FICL')
        NICCO = NAMIND('ICCO')
        NTPCO = NAMIND('TPCO')
        NVDCO = NAMIND('VDCO')
        NVTRP = NAMIND('VTRP')
        NVDXY = NAMIND('VDXY')
        NVDZT = NAMIND('VDZT')
        NRUNR = NAMIND('RUNR')
        NVTMA = NAMIND('VTMA')
        NAVDGC= NAMIND('VDGC')
C
C      set reading of data base VTRP bank
C
        IPRUN  = 0
        IPVSTP = 0
        LDBAS  = JUNIDB(0)
C
C      and set default values of /VTRPAR/
C
        MAXCLS = 100
        MAXCOM = 10
        IVFRFT = 2
        C2PRCL = 8.
        SEACUT = 15.
        CI2CUT = 15.
        PULMIN = 900.
        BIGERR = 1000.
        USNOIS = 11.
        WSNOIS = 13.
        NLYRMX = 4
        ELARP2 = .014**2
        ESPLP2 = .005**2
        DRESIU = .040
        DRESOU = .015
        DRESLW = .015
        DRESOW = .015
        CH2AMB = 5.
C
C      set work banks
C
        KWSRT = 0
        KVTUC = 0
        CALL WBANK (IW,KVTUC,LMHLEN+LVTUCA*MAXCLS,*999)
        IW(KVTUC-3) = INTCHA ('VTUC')
        IW(KVTUC+LMHCOL) = LVTUCA
        IW(KVTUC+LMHROW) = 0
        KVTWC = 0
        CALL WBANK (IW,KVTWC,LMHLEN+LVTWCA*MAXCLS,*999)
        IW(KVTWC-3) = INTCHA('VTWC')
        IW(KVTWC+LMHCOL) = LVTWCA
        IW(KVTWC+LMHROW) = 0
        KVTS0 = 0
        CALL WBANK (IW,KVTS0,LMHLEN+LVTSCA*MAXCOM,*999)
        IW(KVTS0-3) = INTCHA('VTS0')
        IW(KVTS0+LMHCOL) = LVTSCA
        IW(KVTS0+LMHROW) = 0
        KVTS1 = 0
        CALL WBANK (IW,KVTS1,LMHLEN+LVTSCA*MAXCOM,*999)
        IW(KVTS1-3) = INTCHA('VTS1')
        IW(KVTS1+LMHCOL) = LVTSCA
        IW(KVTS1+LMHROW) = 0
      ENDIF
C
C-- reset ouput flags
C
      IGRBMX = 0
      IERVTR = 0
      NMATC(1) = 0
      NMATC(2) = 0
C
C-- parameter bank
C
      CALL ABRUEV (IRUN,IEV)
      IF (IRUN.NE.IPRUN) THEN
         IPRUN = IRUN
         IVSTP = GTSTUP ('VD',IRUN)
      ENDIF
C
      IF (IVSTP.NE.IPVSTP) THEN
         IPVSTP = IVSTP
         IRET = ALGTDB (LDBAS,'VTRPVTCE',-IVSTP)
         KVTRP = IW(NAMIND('VTRP'))
         IF(KVTRP.NE.0) THEN
            JVTRP  = KVTRP + LMHLEN
            MAXCLS = IW(JVTRP+JVTRMC)
            MAXCOM = IW(JVTRP+JVTRML)
            IVFRFT = IW(JVTRP+JVTRFN)
            C2PRCL = RW(JVTRP+JVTRCI)
            SEACUT = RW(JVTRP+JVTRSC)
            CI2CUT = RW(JVTRP+JVTRCC)
            BIGERR = RW(JVTRP+JVTRBE)
            PULMIN = RW(JVTRP+JVTRMP)
            USNOIS = RW(JVTRP+JVTRUS)
            WSNOIS = RW(JVTRP+JVTRWS)
            CALL WBANK (IW,KVTUC,LMHLEN+LVTUCA*MAXCLS,*999)
            CALL WBANK (IW,KVTWC,LMHLEN+LVTWCA*MAXCLS,*999)
            CALL WBANK (IW,KVTS0,LMHLEN+LVTSCA*MAXCOM,*999)
            CALL WBANK (IW,KVTS1,LMHLEN+LVTSCA*MAXCOM,*999)
         ENDIF
         HBIGER = .5*BIGERR
C
         KVTCE = IW(NAMIND('VTCE'))
         IF(KVTCE.NE.0) THEN
            JVTCE  = KVTCE + LMHLEN
            ELARP2 = RW(JVTCE+JVTCEL)**2
            ESPLP2 = RW(JVTCE+JVTCES)**2
            DRESIU = RW(JVTCE+JVTCIU)
            DRESOU = RW(JVTCE+JVTCOU)
            DRESLW = RW(JVTCE+JVTCLW)
            DRESOW = RW(JVTCE+JVTCOW)
            CH2AMB = RW(JVTCE+JVTCAC)
         ENDIF
      ENDIF
C
      CALL BDROP(IW,'VCPLVDCOFVCL')
C
C-- skip event if no TPC+ITC tracks
C
      IF (IW(NFRFT).EQ.0) GO TO 999
C
C-- check if needed banks are there
C
      KFRTL = IW(NFRTL)
      KFRFT = IW(NFRFT)
      IF ( KFRTL.EQ.0) GOTO 998
C
      KFTCL = IW(NFTCL)
      KTPCO = IW(NTPCO)
      IF (KFTCL.EQ.0 .OR. KTPCO.EQ.0) GOTO 998
C
      KITCO = IW(NICCO)
      KFICL = IW(NFICL)
      IF (KITCO.EQ.0 .OR. KFICL.EQ.0) GOTO 998
C
C-- create work banks for tempory storage
C
      KFRFT = IW(NFRFT)
      NTRK  = LROWS(KFRFT)
      CALL WBANK(IW,KWSRT,2*NTRK,*50)
C
C - VTMA is a named bank because it is used by UFTTRK
C
      CALL AUBOS('VTMA',0,LMHLEN+LVTMAA*MAXCOM*MAXCOM,KVTMA,IGARB)
      IGRBMX = MAX(IGARB,IGRBMX)
      IF(IGARB.GE.2) GOTO 997
      IW(KVTMA+LMHCOL) = LVTMAA
      IW(KVTMA+LMHROW) = 0
C
C-- create the VDCO bank
C
      CALL AUBOS('VDCO',0,LMHLEN+LVDCOA*4*NTRK,KVDCO,IGARB)
      IGRBMX = MAX(IGARB,IGRBMX)
      IF(IGARB.GE.2) GOTO 997
      IW(KVDCO+LMHCOL) = LVDCOA
      IW(KVDCO+LMHROW) = 0
C
C-- create the new FRFT bank for TPC+ITC+VDET
C
      CALL AUBOS('FRFT',IVFRFT,LMHLEN+LFRFTA*NTRK,KFRFT,IGARB)
      IGRBMX = MAX(IGARB,IGRBMX)
      IF(IGARB.GE.2) GOTO 997
      LFRFT = IW(NFRFT)
      CALL UCOPY(IW(LFRFT+1),IW(KFRFT+1),LMHLEN+LFRFTA*NTRK)
C
C-- clear VDET info in FRTL
C
      KFRTL = IW(NFRTL)
      DO 70 I=1,NTRK
          IW(KROW(KFRTL,I)+JFRTIV) = 0
 70       IW(KROW(KFRTL,I)+JFRTNV) = 0
C
C-- clear number of associate track in VDXY and VDZT
C
      KVDXY = NVDXY + 1
 75   KVDXY = IW(KVDXY-1)
      IF(KVDXY.NE.0) THEN
        DO 80 ICL = 1,LROWS(KVDXY)
 80       IW(KROW(KVDXY,ICL)+JVDXNA) = 0
        GO TO 75
      ENDIF
      KVDZT = NVDZT + 1
 85   KVDZT = IW(KVDZT-1)
      IF(KVDZT.NE.0) THEN
        DO 90 ICL = 1,LROWS(KVDZT)
 90       IW(KROW(KVDZT,ICL)+JVDZNA) = 0
        GO TO 85
      ENDIF
C
C Also clear NASS in VDGC
C
      KVDGC=IW(NAVDGC)
      IF(KVDGC.NE.0)THEN
        NVDGC=LROWS(KVDGC)
        DO IVDGC=1,NVDGC
          IW(KROW(KVDGC,IVDGC)+JVDGNA)=0
        ENDDO
      ENDIF
C

C-- setup the KALMAN filter to calculate the error matrix AT THE ORIGIN
C
      CALL UFSWIM(ISWIM)
C
C    order tracks by increasing extrapolated errors stored in VTXT
C    bank associated to a track. The bank KWSRT contains this error
C    for tracks with VDET hits (tracks with associated VTXT bank)
C    0. otherwise. These tracks (with no VTXT) will be considered first
C    then the tracks with the lowest errors.
C
      KVTXT = NVTXT+1
  100 KVTXT = IW(KVTXT-1)
      IF (KVTXT.NE.0) THEN
         I = IW(KVTXT-2)
         RW(KWSRT+I) = RTABL (KVTXT,1,JVTXSU)
         GOTO 100
      ENDIF
      CALL SORTZV(IW(KWSRT+1),IW(KWSRT+NTRK+1),NTRK,1,0,0)
C
C-- loop over sorted tracks
C
      DO 500 ITL = 1,NTRK
        JTK = IW(KWSRT+NTRK+ITL)
C
C-- create the VCPL bank for this track
C
        CALL AUBOS('VCPL',JTK,LMHLEN+LVCPLA*NLYRMX,KVCPL,IGARB)
        IGRBMX = MAX(IGARB,IGRBMX)
        IF(IGARB.GE.2) GOTO 997
        IW(KVCPL+LMHCOL) = LVCPLA
        IW(KVCPL+LMHROW) = 0
C
C-- do track cluster linking
C
        CALL VTLINK(JTK)
C
C-- do track refit
C
        KFRFT = NLINK('FRFT',IVFRFT)
        JFRFT = KROW (KFRFT,JTK)
        KFRTL = IW(NFRTL)
        JFRTL = KROW (KFRTL,JTK)
        KVTMA = IW(NVTMA)
        IF(KVTMA.EQ.0) GO TO 450
        IF(LROWS(KVTMA).EQ.0) GO TO 450
C
        KFTCL = IW(NFTCL)
        KTPCO = IW(NTPCO)
        KITCO = IW(NICCO)
        KFICL = IW(NFICL)
        IOITC = IW(JFRTL+JFRTII)
        IOTPC = IW(JFRTL+JFRTIT)
        IOVDT = IW(JFRTL+JFRTIV)
        NTPC  = IW(JFRTL+JFRTNT)
        NITC  = IW(JFRTL+JFRTNI)
C
C-- warning: here we use a negative value for NVDET in order
C            to tell the Kalman filter to do the VDET pattern recognitio
C
        NVDET = -1
C
        NDOF = 2*(NTPC+NITC) -5
        IF (NDOF.LT.1) GO TO 450
C
C-- call the KALMAN filter fitting routine. The CHI2 returned
C         is very large (10**30) if the fit fails.
C
        IERR  = 0
        IOPT  = 9
        NMULT = 0
        CALL UFTTRA(JTK,FIELD,RW(JFRFT+JFRFIR),
     +                  RW(JFRFT+JFRFC2),
     +                  NTPC,NITC,NVDET,IW(KFTCL+LMHLEN+IOTPC+1),
     +                  IW(KFICL+LMHLEN+IOITC+1),0,
     +                  HP,COV,CHI2,NDOF)
        IF (CHI2.GT.1.0E10) GO TO 450
C
C-- fill the VDET-FRFT bank (id=IVFRFT) with the new track parameters.
C   If there is no VDET information for this track we keep the old
C   TPC+ITC track parameters.
C
        DO 300 I=1,6
          RW(JFRFT+JFRFIR-1+I) = HP(I)
  300   CONTINUE
        DO 400 I=1,21
          RW(JFRFT+JFRFEM-1+I) = COV(I)
  400   CONTINUE
        RW(JFRFT+JFRFC2) = CHI2
        IW(JFRFT+JFRFDF) = NDOF
        IW(JFRFT+JFRFNO) = NMULT*100+IOPT*10+IERR
C
C-- fill the VDET-FRTL bank (id=0)
C
        IW(JFRTL+JFRTIV) = IOFVCL
        IW(JFRTL+JFRTNV) = NARCVD
C
C  Drop old VDSM,0 bank
C
        JVDMS=NLINK('VDMS',0)
        IF(JVDMS.GT.0) IW(JVDMS+2)=0
        GO TO 500
C
C-- Swim error matrix to the origin for track without VDET hits
C
  450   CONTINUE
        IF(ISWIM.EQ.0) GO TO 500
        CALL VDMSUP(JTK,0)
        CALL UFTMSO(JTK,COV)
C
C  This is the CHANGE in the covariance matrix
        DO 470 I=1,15
           RW(JFRFT+JFRFEM-1+I) = RW(JFRFT+JFRFEM-1+I) + COV(I)
 470    CONTINUE
C
C  Drop old VDSM,0 bank
C
        JVDMS=NLINK('VDMS',0)
        IF(JVDMS.GT.0) IW(JVDMS+2)=0
C
  500 CONTINUE
C
C-- compress VDCO and VCPL banks
C
      CALL AUBPRS('VDCOVCPL')
C
C-- create and fill FVCL bank.
C
      KVDCO = IW(NVDCO)
      IF (KVDCO.NE.0) THEN
         NCLST = LROWS(KVDCO)
         CALL AUBOS('FVCL',0,LMHLEN+LFVCLA*NCLST,KFVCL,IGARB)
         IGRBMX = MAX(IGARB,IGRBMX)
         IF(IGARB.GE.2) GOTO 997
         IW(KFVCL+LMHCOL) = LFVCLA
         IW(KFVCL+LMHROW) = NCLST
         DO 600 I=1,NCLST
            IW(KFVCL+LMHLEN+I) = I
 600     CONTINUE
      ENDIF
C
C   Reset NASS to a positive value (for new Vdet) and
C-- fill monitoring information
C
      KVDXY = NVDXY + 1
 750  KVDXY = IW(KVDXY-1)
      IF(KVDXY.NE.0) THEN
        JVDXY = KVDXY+LMHLEN
        DO 760 ICL = 1,LROWS(KVDXY)
          IW(JVDXY+JVDXNA)=IABS(IW(JVDXY+JVDXNA))
          NMATC(1) = NMATC(1) + IW(JVDXY+JVDXNA)
          JVDXY = JVDXY + LCOLS(KVDXY)
 760    CONTINUE
        GO TO 750
      ENDIF
      KVDZT = NVDZT + 1
 770  KVDZT = IW(KVDZT-1)
      IF(KVDZT.NE.0) THEN
        JVDZT = KVDZT+LMHLEN
        DO 780 ICL = 1,LROWS(KVDZT)
          IW(JVDZT+JVDZNA)=IABS(IW(JVDZT+JVDZNA))
          NMATC(2) = NMATC(2) + IW(JVDZT+JVDZNA)
          JVDZT = JVDZT + LCOLS(KVDZT)
 780    CONTINUE
        GO TO 770
      ENDIF
C
C Also reset NASS in VDGC
C
      KVDGC=IW(NAVDGC)
      IF(KVDGC.NE.0)THEN
        NVDGC=LROWS(KVDGC)
        DO IVDGC=1,NVDGC
          IW(KROW(KVDGC,IVDGC)+JVDGNA)=IABS(ITABL(KVDGC,IVDGC,JVDGNA))
        ENDDO
      ENDIF
      GO TO 999
C
C - not enough space to book a working bank
 50   IERVTR = 4
      GOTO 999
C - not enough space to book a named bank
 997  IERVTR = 5
      GOTO 999
C - some banks are missing
 998  IERVTR = 6
      GOTO 999
C
  999 IGARB = IGRBMX
      IER   = IERVTR
C
C-- put the error matrix calculation back at the innermost coordinates
C
      CALL UFSWIM(0)
C
C
C-- delete work space
C
      CALL BDROP(IW,'VTMA')
      CALL WDROP(IW,KWSRT)
      RETURN
      END
