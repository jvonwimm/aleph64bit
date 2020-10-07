      SUBROUTINE  VTCLLD(ITK,IL,SCUT,IUORW,NCL)
C-------------------------------------------------------------
C!Return VDET cluster whitin an given area.
CKEY VDET TRACK
C
C  Author     :  B. Mours - 901001
C  modified by : B. Mours - 910918
C    look for cluster in adjacent wafers
C                H.G. Moser - 910918
C    normelise pulseheight before cut, select good clusters
C
C                A. Bonissent March 1995
C          use Vdet year sensitive routime VRMWF
C          to go from wafer to readout module
C          New logic for multitrack assignment and new Vdet
C          (old Vdet unchanged)
C
C  We neglect the error comming from the cluster.
C  This routine give at less a dummy cluster (residual=0, sigma=0)
C
C    ITK   (in)  Track number
C    IL    (in)  Layer number
C    SCUT  (in)  Size of the search area
C    IUORW (in)  = 0 if in xy plan; 1 if in z
C    NCL   (out) Number of found clusters + 1 (dummy cluster)
C-------------------------------------------------------------
      SAVE
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
C   NRR = NR for appropriate view (view included)
      INTEGER NRR,IHIT
      INTEGER NVTUC,NVTWC,KVTXT,KVDXZ,KVTXC,ICL,NASS,JVTXC,IWAF
      REAL    RES,PULSE
      REAL VUW(2),XYZ(3),VEC(3),RERRP(10)
      INTEGER VDYEAR
      INTEGER IERR,IWAF0
      INTEGER IROM,IVIEW
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      LOGICAL FIRSTW
C - bit 1 (IVETOC=2) is set in VDXY and VDZT quality flag to indicate
C   a generic veto hit.
C   bit 30 (IMCNEF=536870912) is set in VDXY and VDZT quality flag
C   to indicate a MC inefficiency rejected hit.
      INTEGER IVETOC,IMCNEF
      PARAMETER (IVETOC=2, IMCNEF=536870912)
      PARAMETER(JVDXXC=1,JVDXYC=2,JVDXUC=3,JVDXSX=4,JVDXSY=5,JVDXSU=6,
     +          JVDXPH=7,JVDXQF=8,JVDXNA=9,JVDXIP=10,JVDXIW=11,
     +          JVDXIH=12,LVDXYA=12)
      PARAMETER(JVDZZC=1,JVDZWC=2,JVDZSZ=3,JVDZSW=4,JVDZPH=5,JVDZQF=6,
     +          JVDZNA=7,JVDZIP=8,JVDZIW=9,JVDZIH=10,LVDZTA=10)
      INTEGER JVDGMD,JVDGVI,JVDGPH,JVDGNA,LVDGCA
      PARAMETER(JVDGMD=1,JVDGVI=2,JVDGPH=3,JVDGNA=4,LVDGCA=4)
      INTEGER JVDMVD,LVDMRA
      PARAMETER(JVDMVD=1,LVDMRA=1)
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JVTUWI=1,JVTUCI=5,JVTUUC=9,JVTUSU=13,JVTURC=17,
     +          JVTUPH=21,JVTURE=25,LVTUCA=28)
      PARAMETER(JVTWWI=1,JVTWCI=5,JVTWWC=9,JVTWSW=13,JVTWZC=17,
     +          JVTWRE=21,LVTWCA=24)
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
C!    local common to store work bank indices
      INTEGER KWSRT,KVTUC,KVTWC,KVTS0,KVTS1
      COMMON /VTBOS/ KWSRT, KVTUC, KVTWC, KVTS0, KVTS1
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVDGC=NAMIND('VDGC')
      ENDIF
      KVTXT = NLINK('VTXT',ITK)
      IF(KVTXT.EQ.0) GO TO 999
C
      NCL = 1
      IWAF = ITABL(KVTXT,IL,JVTXWI)
      FIRSTW = .TRUE.
      IMASK = IVETOC+IMCNEF
      VUW(1) = RTABL(KVTXT,IL,JVTXUC)
      VUW(2) = RTABL(KVTXT,IL,JVTXWC)
C
   50 CONTINUE
C
      IVIEW=2-IUORW
      CALL VRMWF(IWAF,IVIEW,IROM)
      IWAF=IROM
      IF(IUORW.EQ.0) THEN
        KVDXZ = NLINK('VDXY',IWAF)
        KVTXC = KVTUC
        NRR=IWAF+1
      ELSE
        KVDXZ = NLINK('VDZT',IWAF)
        KVTXC = KVTWC
        NRR=IWAF
      ENDIF
      IF(KVDXZ.EQ.0) GO TO 900
C
C     pulseheight correction factor for inclined tracks
C
      PTOT = SQRT( RTABL(KVTXT,IL,JVTXPV)**2 +
     +             RTABL(KVTXT,IL,JVTXPU)**2 +
     +             RTABL(KVTXT,IL,JVTXPW)**2 )
      PCOR = ABS(RTABL(KVTXT,IL,JVTXPV)) / PTOT
C
C-- Loop over all cluster, keep only the close one
C
      DO 100 ICL = 1,LROWS(KVDXZ)
        JVTXC = KROW(KVTXC,NCL)
        IF(IUORW.EQ.0) THEN
          RES   = RTABL(KVDXZ,ICL,JVDXUC) - VUW(1)
          NASS  = ITABL(KVDXZ,ICL,JVDXNA)
          PULSE = RTABL(KVDXZ,ICL,JVDXPH) * PCOR
          IQFL  = ITABL(KVDXZ,ICL,JVDXQF)
          RW(JVTXC+JVTURE+IL-1) = RES
        ELSE
          RES   = RTABL(KVDXZ,ICL,JVDZWC) - VUW(2)
          NASS  = ITABL(KVDXZ,ICL,JVDZNA)
          PULSE = RTABL(KVDXZ,ICL,JVDZPH) * PCOR
          IQFL  = ITABL(KVDXZ,ICL,JVDZQF)
          RW(JVTXC+JVTWRE+IL-1) = RES
        ENDIF
        IF(IAND(IQFL,IMASK).NE.0)       GO TO 100
        IF(ABS(RES).GT.SCUT)            GO TO 100
        IF(VDYEAR().NE.95)THEN
           IF(PULSE.LT.FLOAT(NASS)*PULMIN) GO TO 100
        ELSE
C
C NASS<0 means that tracks have already been associated but
C pulseheight is large enough for one more track
C Therefore we only need to test against >0, which means no more
C tracks can be given
C This is set in VTFILL
C
           IF(NASS.GT.0)GO TO 100
           KVDGC=IW(NAVDGC)
           KVDMR=NLINK('VDMR',NRR)
C Find the relation to global cluster
           IVDGC=ITABL(KVDMR,ICL,JVDMVD)
           NASG =ITABL(KVDGC,IVDGC,JVDGNA)
           IF(NASG.GT.0)GO TO 100
        ENDIF
C
        IF(NCL.GE.MAXCLS) GO TO 999
        IW(JVTXC+JVTUWI+IL-1) = IWAF
        IW(JVTXC+JVTUCI+IL-1) = ICL
        IF(IUORW.EQ.0) THEN
          RW(JVTXC+JVTURC+IL-1) = SQRT( RTABL(KVDXZ,ICL,JVDXXC)**2+
     +                                  RTABL(KVDXZ,ICL,JVDXYC)**2)
          RW(JVTXC+JVTUPH+IL-1) = ATAN2(RTABL(KVDXZ,ICL,JVDXYC),
     +                                  RTABL(KVDXZ,ICL,JVDXXC))
          RW(JVTXC+JVTUUC+IL-1) =       RTABL(KVDXZ,ICL,JVDXUC)
          RW(JVTXC+JVTUSU+IL-1) =       RTABL(KVDXZ,ICL,JVDXSU)**2
        ELSE
          RW(JVTXC+JVTWZC+IL-1) = RTABL(KVDXZ,ICL,JVDZZC)
          RW(JVTXC+JVTWWC+IL-1) = RTABL(KVDXZ,ICL,JVDZWC)
          RW(JVTXC+JVTWSW+IL-1) = RTABL(KVDXZ,ICL,JVDZSW)**2
        ENDIF
        NCL = NCL+1
  100 CONTINUE
C
  900 CONTINUE
C
C-- look for cluster in the adjacent z wafer (only if xy)
C
      IF(FIRSTW) IWAF0 = IWAF
      IF(ITABL(KVTXT,IL,JVTXHF).NE.0 .AND. FIRSTW) THEN
        FIRSTW = .FALSE.
        KFRFT = NLINK('FRFT',0)
        IWAFO = ITABL(KVTXT,IL,JVTXWI)
        IF(VUW(2).LT.0.) THEN
          IWAF = IWAFO-1000
        ELSE
          IWAF = IWAFO+1000
        ENDIF
        IZED  = MOD(IWAF/1000,10)
        IZEDO = MOD(IWAFO/1000,10)
C
C The definition of neighbouring wafers is different for new VDET
C
        IF(VDYEAR().NE.95)THEN
          IF(IWAF.GE.0 .AND. IZED.LE.3 .AND.
     +      (IUORW.NE.0 .OR. (IZED-1.5)*(IZEDO-1.5).LT.0.)) THEN
            CALL VTXNWT(IWAF,RW(KROW(KFRFT,ITK)+JFRFIR),
     +                     RW(KROW(KVTXT,IL)+JVTXXC),
     +                  VUW,XYZ,VEC,RERRP,IERR)
            IF(IERR.EQ.0) GO TO 50
          ENDIF
        ELSE
C This is new VDET. We always look in adjacent wafer.
C  5 is (number of wafers in a face)-1
          IF(IWAF.GE.0.AND.IZED.GE.0 .AND. IZED.LE.5) THEN
            CALL VTXNWT(IWAF,RW(KROW(KFRFT,ITK)+JFRFIR),
     +                     RW(KROW(KVTXT,IL)+JVTXXC),
     +                  VUW,XYZ,VEC,RERRP,IERR)
            IF(IERR.EQ.0) GO TO 50
          ENDIF
        ENDIF
      ENDIF
      IWAF = IWAF0
C
C  Fill dummy cluster
C
      JVTXC = KROW(KVTXC,NCL)
      IW(JVTXC+JVTUWI+IL-1) = IWAF
      IW(JVTXC+JVTUCI+IL-1) = 0
      IF(IUORW.EQ.0) THEN
        RW(JVTXC+JVTURE+IL-1) = 0.
        RW(JVTXC+JVTURC+IL-1) = SQRT( RTABL(KVTXT,IL,JVTXXC)**2+
     +                                RTABL(KVTXT,IL,JVTXYC)**2)
        RW(JVTXC+JVTUPH+IL-1) = ATAN2(RTABL(KVTXT,IL,JVTXYC),
     +                                RTABL(KVTXT,IL,JVTXXC))
        RW(JVTXC+JVTUUC+IL-1) =       RTABL(KVTXT,IL,JVTXUC)
        RW(JVTXC+JVTUSU+IL-1) = BIGERR
      ELSE
        RW(JVTXC+JVTWRE+IL-1) = 0.
        RW(JVTXC+JVTWZC+IL-1) = RTABL(KVTXT,IL,JVTXZC)
        RW(JVTXC+JVTWWC+IL-1) = RTABL(KVTXT,IL,JVTXWC)
        RW(JVTXC+JVTWSW+IL-1) = BIGERR
      ENDIF
C
  999 CONTINUE
      IW(KVTXC+LMHROW) = MAX(NCL,IW(KVTXC+LMHROW))
C
      RETURN
      END
