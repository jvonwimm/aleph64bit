      SUBROUTINE VTFILL (ITK,ICOMB)
C----------------------------------------------------------------------
C! Fill VDCO and VCPL  banks.
CKEY VDET TRACK
C
C  Author      : B. Mours   901001
C  modified by : TSM        910918
C     track ambiguity
C  modified by : B. Mours   911023
C     dont store in VCPL rejected hit (large error)
C  modified by : A. Bonissent 950714
C     new logic for multitrack assignment for new Vdet only
C     Old Vdet unchanged
C
C  input : ITK   = track number
C          ICOMB = combinaison number in VTMA bank
C----------------------------------------------------------------------
      SAVE
C
      PARAMETER(JVDCWI=1,JVDCR0=2,JVDCPH=3,JVDCZ0=4,JVDCSR=5,JVDCSZ=6,
     +          JVDCQF=7,JVDCTN=8,LVDCOA=8)
      PARAMETER(JVDXXC=1,JVDXYC=2,JVDXUC=3,JVDXSX=4,JVDXSY=5,JVDXSU=6,
     +          JVDXPH=7,JVDXQF=8,JVDXNA=9,JVDXIP=10,JVDXIW=11,
     +          JVDXIH=12,LVDXYA=12)
      PARAMETER(JVDZZC=1,JVDZWC=2,JVDZSZ=3,JVDZSW=4,JVDZPH=5,JVDZQF=6,
     +          JVDZNA=7,JVDZIP=8,JVDZIW=9,JVDZIH=10,LVDZTA=10)
      INTEGER JVDMVD,LVDMRA
      PARAMETER(JVDMVD=1,LVDMRA=1)
      INTEGER JVDGMD,JVDGVI,JVDGPH,JVDGNA,LVDGCA
      PARAMETER(JVDGMD=1,JVDGVI=2,JVDGPH=3,JVDGNA=4,LVDGCA=4)
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
      PARAMETER(JVTMNL=1,JVTMNU=2,JVTMNW=3,JVTMC2=4,JVTMIT=5,JVTMFR=6,
     +          JVTMUW=7,JVTMWW=11,JVTMIU=15,JVTMIW=19,JVTMWI=23,
     +          JVTMR0=27,JVTMPH=31,JVTMZ0=35,JVTMUC=39,JVTMWC=43,
     +          JVTMSU=47,JVTMSW=51,JVTMCO=55,LVTMAA=58)
      PARAMETER(JVCPXB=1,JVCPNX=2,JVCPZB=3,JVCPNZ=4,JVCPC2=5,LVCPLA=5)
C!    local common to store work bank indices
      INTEGER KWSRT,KVTUC,KVTWC,KVTS0,KVTS1
      COMMON /VTBOS/ KWSRT, KVTUC, KVTWC, KVTS0, KVTS1
      COMMON /VTKREC/  NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL
     +                ,NARCVD
      INTEGER NLAYER,NULINK,NWLINK,IGRBMX,IERVTR,IOFVCL,NARCVD
      COMMON /VTRPAR/ MAXCLS,MAXCOM,IVFRFT,C2PRCL,SEACUT,CI2CUT,
     +                BIGERR,PULMIN,USNOIS,WSNOIS,HBIGER,NLYRMX
     +    ,           ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB
      INTEGER MAXCOM,MAXCLS,IVFRFT,NLYRMX
      REAL C2PRCL,SEACUT,CI2CUT,BIGERR,PULMIN,HBIGER
      REAL ELARP2,ESPLP2,DRESIU,DRESOU,DRESLW,DRESOW,CH2AMB
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER NFRFT,NVDCO,KFRFT,KVDCO,NVTMA,KVTMA,
     +        JVTMA,IL,IVDCO,JVDCO,JVCPL,KVDXY,ICL,KVDZT
      REAL    SIGMU,SIGMZ
      LOGICAL FIRST
C-- Local variables for ambiguous pattern search
      LOGICAL WAMBIG,UAMBIG,WFOUND,UFOUND
      INTEGER JL,IWFW,IWFU,ICLW,ICLU
C   NRR = NR for appropriate view (view included)
      INTEGER NRR(2),IHIT(2),NASS(2)
      INTEGER VDYEAR
      REAL CBAUG,CPAUG
C -- bit 2 (IVPAMB=4) is set in the VDCO quality flag to indicate
C    a R-Phi ambiguous hit.
C    bit 3 (IVZAMB=8) is set in the VDCO quality flag to indicate
C    a Z ambiguous hit.
      INTEGER IVPAMB,IVZAMB
      PARAMETER (IVPAMB=4, IVZAMB=8)
      DATA FIRST/.TRUE./
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
      IF(FIRST) THEN
        FIRST = .FALSE.
        NVDCO = NAMIND('VDCO')
        NVTMA = NAMIND('VTMA')
        NAVDGC=NAMIND('VDGC')
      ENDIF
C
      KVTMA = IW(NVTMA)
      IF(KVTMA.EQ.0)        GO TO 999
      IF(LROWS(KVTMA).EQ.0) GO TO 999
C
      KVCPL = NLINK('VCPL',ITK)
      KVDCO = IW(NVDCO)
C
C-- loop over all clusters found for this track
C
      JVTMA = KROW(KVTMA,ICOMB)
      IF(IW(JVTMA+JVTMNL).GT.NLYRMX) THEN
          IERVTR = 12
          GO TO 999
      ENDIF
C
C-- Check other VTMA patterns for ambiguity
C   Ambiguity means the AUGMENTED chisquare of another pattern
C   (which includes a penalty for using fewer hits)
C   is not significantly more than the AUGMENTED chisquare
C   of the "best" pattern
C   (significantly means more than CH2AMB units larger)
C
C-- Initialize flags
      WAMBIG=.FALSE.
      UAMBIG=.FALSE.
C-- Get "best" augmented chisquare
      CBAUG=RW(KROW(KVTMA,ICOMB)+JVTMFR)
C-- Loop over patterns
      DO 70 JCOMB = 1, LROWS(KVTMA)
C-- Save time
        IF (WAMBIG .AND. UAMBIG) GO TO 70
C-- Don't compare pattern to itself
        IF (JCOMB .EQ. ICOMB) GO TO 70
C-- Get augmented chisquare of this pattern
        CPAUG=RW(KROW(KVTMA,JCOMB)+JVTMFR)
C-- Compare
        IF (CPAUG .GT. CBAUG+CH2AMB) GO TO 70
C-- We have found an ambiguous pattern
C-- The following stuff is only necessary to determine if the ambiguity
C--   is in phi, z, or both
C-- Loop over layers in ICOMB
        DO 60 IL = 1, IW(JVTMA+JVTMNL)
C-- Save time
          IF (WAMBIG .AND. UAMBIG) GO TO 60
C-- Find wafer and cluster numbers
          IWFW=IW(JVTMA+IL-1+JVTMWW)
          ICLW=IW(JVTMA+IL-1+JVTMIW)
          IWFU=IW(JVTMA+IL-1+JVTMUW)
          ICLU=IW(JVTMA+IL-1+JVTMIU)
C-- Initialize flags for layer hit search
          WFOUND=.FALSE.
          UFOUND=.FALSE.
C-- Loop over layers in JCOMB
          DO 50 JL = 1, IW(KROW(KVTMA,JCOMB)+JVTMNL)
C-- Save time
            IF (.NOT. WFOUND) THEN
C-- Check if same W wafer
              IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMWW) .EQ. IWFW) THEN
C-- Set flag if cluster number matches or alternate is null hit
                IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMIW) .EQ. ICLW .OR.
     >                 RW(KROW(KVTMA,JCOMB)+JL-1+JVTMSW).GT.HBIGER) THEN
                  WFOUND=.TRUE.
                ENDIF
              ENDIF
            ENDIF
C-- Same for U hit
            IF (.NOT. UFOUND) THEN
              IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMUW) .EQ. IWFU) THEN
                IF (IW(KROW(KVTMA,JCOMB)+JL-1+JVTMIU) .EQ. ICLU .OR.
     >                 RW(KROW(KVTMA,JCOMB)+JL-1+JVTMSU).GT.HBIGER) THEN
                  UFOUND=.TRUE.
                ENDIF
              ENDIF
            ENDIF
C-- End loop over JCOMB layers
   50     CONTINUE
C-- Update ambiguity flags
          UAMBIG=UAMBIG .OR. .NOT. UFOUND
          WAMBIG=WAMBIG .OR. .NOT. WFOUND
C       (i.e. Ambiguous if the hit on the best track was NOT
C               present on the 2nd best track)
C
C-- End loop over ICOMB layers
   60   CONTINUE
C-- End loop over JCOMB=other VTMA patterns
   70 CONTINUE
C-- WAMBIG and UAMBIG now contain the ambiguity information
      NARCVD = 0
C
      DO 100 IL=1,IW(JVTMA+JVTMNL)
        SIGMU = RW(JVTMA+JVTMSU+IL-1)
        SIGMZ = RW(JVTMA+JVTMSW+IL-1)
        IF(SIGMU.GT.HBIGER .AND. SIGMZ.GT.HBIGER)  GO TO 100
        NARCVD = NARCVD + 1
        IF(NARCVD.EQ.1) IOFVCL = LROWS(KVDCO)
C
C-- fill VDCO bank
C
        JVDCO = KNEXT(KVDCO)
        IW(JVDCO+JVDCWI) = IW(JVTMA+JVTMWW+IL-1)
        RW(JVDCO+JVDCR0) = RW(JVTMA+JVTMR0+IL-1)
        RW(JVDCO+JVDCPH) = RW(JVTMA+JVTMPH+IL-1)
        RW(JVDCO+JVDCZ0) = RW(JVTMA+JVTMZ0+IL-1)
        RW(JVDCO+JVDCSR) = SIGMU
        RW(JVDCO+JVDCSZ) = SIGMZ
        IW(JVDCO+JVDCQF) = 3
        IF(SIGMU.GT.HBIGER) IW(JVDCO+JVDCQF) = IW(JVDCO+JVDCQF) - 1
        IF(SIGMZ.GT.HBIGER) IW(JVDCO+JVDCQF) = IW(JVDCO+JVDCQF) - 2
C-- Set ambiguity bits
        IF (UAMBIG) IW(JVDCO+JVDCQF)=IOR(IW(JVDCO+JVDCQF),IVPAMB)
        IF (WAMBIG) IW(JVDCO+JVDCQF)=IOR(IW(JVDCO+JVDCQF),IVZAMB)
        IW(JVDCO+JVDCTN) = ITK
        IW(KVDCO+LMHROW) = LROWS(KVDCO) + 1
C
C-- fill the VCPL bank
C
        JVCPL = KNEXT(KVCPL)
        IW(JVCPL+JVCPXB) = IW(JVTMA+JVTMUW+IL-1)
        IW(JVCPL+JVCPZB) = IW(JVTMA+JVTMWW+IL-1)
        IF(SIGMU.LT.HBIGER) IW(JVCPL+JVCPNX) = IW(JVTMA+JVTMIU+IL-1)
        IF(SIGMZ.LT.HBIGER) IW(JVCPL+JVCPNZ) = IW(JVTMA+JVTMIW+IL-1)
        RW(JVCPL+JVCPC2) = 0.
        IW(KVCPL+LMHROW) = LROWS(KVCPL) + 1
        IF(VDYEAR().NE.95)THEN
C
C-- update Nass in VDXT and VDZT
C
          KVDXY = NLINK('VDXY',IW(JVCPL+JVCPXB))
          IF(KVDXY.NE.0) THEN
            ICL = IW(JVCPL+JVCPNX)
            IHIT(2)=ICL
            IF(ICL.NE.0) IW(KROW(KVDXY,ICL)+JVDXNA) =
     +                 IW(KROW(KVDXY,ICL)+JVDXNA) + 1
          ENDIF
          KVDZT = NLINK('VDZT',IW(JVCPL+JVCPZB))
          IF(KVDZT.NE.0) THEN
            ICL = IW(JVCPL+JVCPNZ)
            IHIT(1)=ICL
            IF(ICL.NE.0) IW(KROW(KVDZT,ICL)+JVDZNA) =
     +                 IW(KROW(KVDZT,ICL)+JVDZNA) + 1
          ENDIF
C
        ELSE
C
C For Vdet95, update number of associated tracks at both levels
C
C
C     pulseheight correction factor for inclined tracks
C
          KVTXT = NLINK('VTXT',ITK)
          PTOT = SQRT( RTABL(KVTXT,IL,JVTXPV)**2 +
     +                 RTABL(KVTXT,IL,JVTXPU)**2 +
     +                 RTABL(KVTXT,IL,JVTXPW)**2 )
          PCOR = ABS(RTABL(KVTXT,IL,JVTXPV)) / PTOT
C
C Access the hit banks and get info
C
          KVDXY = NLINK('VDXY',IW(JVCPL+JVCPXB))
          NRR(2)=IW(JVCPL+JVCPXB)+1
          IHIT(2)=0
          IF(KVDXY.NE.0) THEN
            ICL = IW(JVCPL+JVCPNX)
            IHIT(2)=ICL
            IF(ICL.NE.0)NASS(2)=ITABL(KVDXY,ICL,JVDXNA)
          ENDIF
          KVDZT = NLINK('VDZT',IW(JVCPL+JVCPZB))
          NRR(1)=IW(JVCPL+JVCPZB)
          IHIT(1)=0
          IF(KVDZT.NE.0) THEN
            ICL = IW(JVCPL+JVCPNZ)
            IHIT(1)=ICL
            IF(ICL.NE.0)NASS(1)=ITABL(KVDZT,ICL,JVDZNA)
          ENDIF
C
C Now update nb of tracks
C The algorithm is (for local or global cluster) :
C      if 0 track associated -> 1 if pulseheight compatible only with 1
C                              -1 if large pulseheight, and a
C                                  second track can be associated later
C      else -> NASS=2
C Never more than 2 tracks will be associated
C
          KVDGC=IW(NAVDGC)
          DO IV=1,2
            IF(IV.EQ.1)THEN
              KBNK=KVDZT
              JNAS=JVDZNA
              JPULS=JVDZPH
            ELSE
              KBNK=KVDXY
              JNAS=JVDXNA
              JPULS=JVDXPH
            ENDIF
            ICL=IHIT(IV)
            IF(ICL.NE.0)THEN
              PULS=RTABL(KBNK,ICL,JPULS)*PCOR
              IF(NASS(IV).EQ.0)THEN
                NASS(IV)=1
                IF(PULS.GT.PULMIN)NASS(IV)=-1
              ELSE
                NASS(IV)=IABS(NASS(IV))+1
              ENDIF
              IW(KROW(KBNK,ICL)+JNAS)=NASS(IV)
C Find the relation to global cluster
              KVDMR=NLINK('VDMR',NRR(IV))
              IVDGC=ITABL(KVDMR,ICL,JVDMVD)
              PULS=RTABL(KVDGC,IVDGC,JVDGPH)*PCOR
              NAS =ITABL(KVDGC,IVDGC,JVDGNA)
              IF(NAS.EQ.0)THEN
                NAS=1
                IF(PULS.GT.PULMIN)NAS=-1
              ELSE
                NAS=IABS(NAS)+1
              ENDIF
              IW(KROW(KVDGC,IVDGC)+JVDGNA)=NAS
            ENDIF
          ENDDO
        ENDIF
  100   CONTINUE
C
  999 RETURN
      END
