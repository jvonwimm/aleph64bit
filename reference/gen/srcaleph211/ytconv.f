      SUBROUTINE YTCONV(IER)
C
C----------------------------------------------------------*
C!    Reconstruct Gamma Conversions
CKEY YTOP
C!    Author :     G. Lutz   30/11/87
C!    Modified :   M. Bosman 01/12/88
C!    Rewritten:   G. Lutz    9/02/91
C!    Modified :   G. Lutz   30/03/92
C!    MODIFIED :   G. LUTZ    3/12/92
C!
C!
C!    Description
C!    ===========
C!    This routine looks for gamma conversions
C!    within charged tracks identified as being electrons
C!
C!---------------------------------------------------------*
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JFRIBP=1,JFRIDZ=2,JFRIBC=3,JFRIDC=4,JFRIPE=5,JFRIPM=6,
     +          JFRIPI=7,JFRIPK=8,JFRIPP=9,JFRINK=10,JFRIQF=11,
     +          LFRIDA=11)
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
      PARAMETER(JPYETY=1,JPYEVX=2,JPYEVY=3,JPYEVZ=4,JPYEVM=5,JPYEC2=11,
     +          JPYEDF=12,LPYERA=12)
      PARAMETER(JPYFTN=1,JPYFVN=2,LPYFRA=2)
      PARAMETER(JYNFMO=1,JYNFTL=2,JYNFP0=3,JYNFD0=4,JYNFZ0=5,JYNFEM=6,
     +          JYNFC2=21,JYNFDF=22,JYNFCH=23,JYNFND=24,JYNFPT=25,
     +          JYNFNM=26,JYNFPM=27,JYNFPV=28,JYNFPC=29,LYNFTA=29)
      PARAMETER(JYNMMT=1,JYNMPA=2,JYNMMA=3,JYNMEM=4,
     &     JYNMVD=5,JYNMSC=6,JYNMIC=7,JYNMNA=8,JYNMPR=9,LYNMAA=10)
      PARAMETER(JYNPPX=1,JYNPPY=2,JYNPPZ=3,LYNPEA=3)
      PARAMETER(JYNTMT=1,JYNTDT=2,JYNTTT=3,JYNTMA=4,
     &     JYNTBP=5,JYNTIM=6,JYNTPI=7,LYNTRA=7)
C! beam crossing position
      COMMON/YBCRTO/BCROSS(3),VBCROS(6)
C! YTOP array dimensions
      PARAMETER(NMSIZZ=31,MKDIMM=6,MAXTRK=186,MAXHLX=124,MAXNTR=62,
     &  MXVTOP=10,MXMULS=10,MAXVXP=50,MAXEXC=60,MXVTYP=2)
C! YTOP particle masses
      PARAMETER(    JPAFEP=1,JPAFEM=2,JPAFMP=3,JPAFMM=4,
     &              JPAFPP=5,JPAFPM=6,JPAFKP=7,JPAFKM=8,
     &              JPAFPR=9,JPAFPB=10,JPAFPH=11,JPAFPZ=12,
     &              JPAFKZ=13,JPAFLA=14,JPAFLB=15   )
      COMMON/YPMASS/ YPMASS(20)
C! YTOP parameters
      COMMON/YPARTO/DHXLIM,CHISEL,PMINSE,PIDACP,BFIELD,
     &       MNTHPV,MXTSPV, PMINRQ,PMINRA,
     &       CHVXCO,CHPTCO,RVACCO,AMCTCO,DZMXCO,NAMXCO,EPLOCO,EPHICO,
     &       CHVXV0,CHPTV0,CHVSV0,CHMLV0,DZMXV0,NAMXV0,
     &       PIPKV0,PRPLV0,PIPLV0,
     &       LBCRFD,LRYOLD,LRFRF2,
     &       LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
      LOGICAL LBCRFD,LRYOLD,LRFRF2
      LOGICAL LRPVTX,LRSVTX,LVBCR0,LRMVPV,LRLPVX,LCONVS,LVZERS,LRUSER
C! YTOP summary
      COMMON /YSUMTO/NEPVTO,NRPVTO,ATPVTO,AMPVTO,ACPVTO,APPVTO(3),
     +               NRCOTO,AMCOTO,ACCOTO,ARCOTO,
     +               NRK0TO,AMK0TO,ACK0TO,ATK0TO,
     +               NRLATO,AMLATO,ACLATO,ATLATO,
     +               NRLBTO,AMLBTO,ACLBTO,ATLBTO,
     +               KYFLAG(20)
C! YTOP vertex parameters
      COMMON/YVTXTO/ NEVTOP,NVXTOP,NVXLOT,
     &  VXTOP0(3,MXVTOP,MXVTYP),VVXTOP(6,MXVTOP,MXVTYP),
     &  CHVTOP(MXVTOP,MXVTYP),DRVTOP(MXVTOP,MXVTYP),
     &  MULTVX(MXVTOP,MXVTYP),
     &  IXTRVT(MKDIMM,MXVTOP,MXVTYP),IXVQLI(MAXTRK,MXVTOP),
     &  CHIVXI(MAXTRK,MXVTOP),
     &  NLOTRK,ILOTRK(MAXTRK),
     &  JTRVXA(MAXTRK,MXVTYP),NTRVXA(MAXTRK,MXVTYP),
     &  NEVVTX(3,MXVTOP),NMULVX(MXMULS,MXVTYP),
     &  NMULTA(MXVTOP,MXVTYP),MULMAT(MXMULS,MXMULS)
C! YTOP track parameters
      COMMON/YTRKTO/NGTRTO,IPTRTO(MAXTRK),
     &              PTRECT(MAXTRK),PRECTO(MAXTRK),
     &              KPIDF0(MAXTRK),KPORF0(MAXTRK)
C
C
C
C     GAMMA CONV. SEARCH
      REAL XYZCO(3,4),RADC1(4),RADC2(4),FIICO(4),ZETCO(4)
      DIMENSION AMPC(2),IXHX(2),VTX2(3),VARV2(6)
      DIMENSION IPTR(MAXTRK)
      DIMENSION AMHX(2),VXOUT(3),VVXOU(6),HXOU(5,2),VHXOU(15,2),
     &  PSUM(3),VPSUM(6),VPSVX(3,3),VMVX(3),VMPS(3)
      DIMENSION VXOU2(3),VVXO2(6)
      DIMENSION IXNU(2),TNUO(5,2),VTNUO(15,2)
      DIMENSION TRACK(5),VTRACK(15)
      DIMENSION VHXIN(15,MAXTRK,2)
      REAL XYZVD(3,4),RADVD(4),FVDCO(4),ZETVD(4)
      REAL XYZIT(3,20),RADIT(20),FITCO(20),ZETIT(20)
      DIMENSION NVDF(2),NVDB(2),NITF(2),NITB(2)
C
      LOGICAL LVAPC
      LOGICAL LMRK
      LOGICAL LGARB
      LOGICAL LCONMC
C
      LOGICAL LDMP1,LDMP2
      LOGICAL LFIRST
C
      DIMENSION ITADD(MAXTRK)
C     MAX. # OF ADDITIONAL TRACKS PASSING THROUGH CONV. VERTEX
      DATA LFIRST/.TRUE./
      DATA LDMP1/.FALSE./,LDMP2/.FALSE./
C
C     dimension of buffer for track errors
      DATA LVHXIN / 15/
C
C     maximum chisq for vertex candidates
C     maximum chisq for pointing chisq of gamma to BCRO
C     radius of vacuum
C     cut on the mass of the reconstructed particle
C     Maximum Z-distance of tracks from beam crossing
C
C     LOGICAL FLAG FOR MASS CONSTRAINT
      DATA LCONMC/.FALSE./
C
C     MASS CHISQ LIMIT FROM OPENING ANGLE AND MASS RES.
C     DATA CHML/9./
C
C     calculate approx. vtx in YFTHVX
      DATA LVAPC/.TRUE./
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
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
      IF(LFIRST) THEN
C     GENERATE MARKERS
        CALL YMKZER(1,NMSIZZ,MKEP)
        CALL YMKSET(1,NMSIZZ,MKEP,JPAFEP)
        CALL YMKSET(1,NMSIZZ,MKEP,JPAFEM)
        CALL YMKZER(1,NMSIZZ,MKPH)
        CALL YMKSET(1,NMSIZZ,MKPH,JPAFPH)
        LFIRST=.FALSE.
      ENDIF
C
C
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
      IF(IFAIL.NE.0) GOTO 999
C
      KFRID=IW(NAMIND('FRID'))
C
      ICCON=ICCON+1
C
C
C
      IER = 0
C
C-- Add multiple scattering component to ITC-TPC track errors
C-- in case we deal with FRFT bank 0
C-- and copy them to an array
C
C multiple scattering in THE FOLLOWING SECTION is slightly incorrect
C when e.g. vertex is in ITC but no ITC hits are found (FRFT0 gets
C errors at first measured point on track)
      IF(KFRFT0.EQ.0) THEN
        DO I=1,LROWS(KFRFT)
          IF(I.LE.MAXTRK) THEN
            IFRFT=KROW(KFRFT,I)
            DO J=1,15
              VHXIN(J,I,1)=RW(IFRFT+JFRFEM+J-1)
            ENDDO
          ENDIF
        ENDDO
      ELSE
        DO 40 I=1,LROWS(KFRFT)
          IF(I.GT.MAXTRK) GOTO 40
          IFRFT=KROW(KFRFT,I)
          DO 41 J=1,15
   41     VHXIN(J,I,1)=RW(IFRFT+JFRFEM+J-1)
          IF(KFRFT.EQ.KFRFT0) THEN
            NITC=ITABL(KFRTL,I,JFRTNI)
            CALL UMSERR(1,NITC,0,RW(KFRFT+LMHLEN+JFRFIR),
     &                VHXIN(1,I,1),VHXIN(1,I,1),IFAIL)
            IF(IFAIL.NE.0) GOTO 998
          ENDIF
   40   CONTINUE
      ENDIF
C
C-- save ITC-TPC-VDET track errors
C-- check that VDET track bank exists
      IF(KFRFT.EQ.KFRFT0) GOTO 44
      DO 42 I=1,LROWS(KFRFT)
        IFRFT=KROW(KFRFT,I)
        IF(I.GT.MAXTRK) GOTO 42
        DO 43 J=1,15
   43   VHXIN(J,I,2)=RW(IFRFT+JFRFEM+J-1)
   42 CONTINUE
   44 CONTINUE
C
C find gamma conversions
C     select tracks compatible with e+- assignement
      II=0
      DO 50 K=1,NGTRTO
        I=IPTRTO(K)
        IF(I.EQ.0) GOTO 50
C     reject itc only tracks
        KODE=1
        IF(ITABL(KFRTL,I,JFRTNT)+ITABL(KFRTL,I,JFRTNV).EQ.0) GO TO 49
C     reject tracks with large z-distance from beam crossing
        Z0=RTABL(KFRFT,I,JFRFZ0)
        DZ0=SQRT(RTABL(KFRFT,I,JFRFEM+14))
        KODE=3
        IF(ABS(Z0).GT.(DZMXCO+3.*DZ0+3*SQRT(VBCROS(6)))) GO TO 50
C     reject non electrons
        KODE=4
        CALL YMKAND(1,NMSIZZ,MKEP,KPIDF0(I),IDUM,LMRK)
        IF(.NOT.LMRK) THEN
          II=II+1
          IPTR(II)=IPTRTO(K)
        ELSE
          GO TO 49
        ENDIF
        GO TO 50
   49   CONTINUE
   50 CONTINUE
C
C     LOOP OVER PAIRS WITH OPPOSITE CHARGE
      IF(II.LT.2) GO TO 61
      II1=II-1
      DO 60 I=1,II1
        I1=I+1
        IIP=IPTR(I)
        DO 60 J=I1,II
          IP=IIP
          JJP=IPTR(J)
          JP=JJP
C       require opposite charge
          IF(PRECTO(IP)*PRECTO(JP).GT.0.) GO TO 60
          IF(PRECTO(IP).LT.0.) THEN
C     TAKE POSITIVE PARTICLE FIRST
            IP=JJP
            JP=IIP
          ENDIF
          IXHX(1)=IP
          IXHX(2)=JP
C
C     electron probability
          ELPR1=RTABL(KFRID,IP,JFRIPE)
          ELPR2=RTABL(KFRID,JP,JFRIPE)
          IF(AMIN1(ELPR1,ELPR2).LT.EPLOCO.OR.
     &      AMAX1(ELPR1,ELPR2).LT.EPHICO) THEN
            GO TO 60
          ENDIF
C
C  FIT VERTEX ONLY
          KFRFTU=KFRFT
          IF(KFRFT0.NE.0) KFRFTU=KFRFT0
C CHECK IF VDET TRACKS AVAILABLE
          KMAL=2
          IF(KFRFT.EQ.KFRFT0) KMAL=1
          KOFRF=1
          DO 160 MAL=1,KMAL
C  FIRST FIT VERTEX WITH FRFT NUMBER 0 IE TRACKS WITHOUT VDET HITS
C     conversion vertex
            CALL YFTVTR(0,2,0,LVAPC,DUM,DUMY,IXHX,
     &        LCOLS(KFRFTU),LVHXIN,
     &        RW(KFRFTU+LMHLEN+JFRFIR),VHXIN(1,1,MAL),
     &        IDUM,IDUM,IDUM,DUM,DUM,VTX2,VARV2,CHVX2,IFAIL)
C
C NOW LOOK IF WE HAVE VDET HITS BEHIND THE VERTEX
C BUT ONLY IN THE CASE WE REQUEST USING FRFT nr 2 BANK
            IF(MAL.EQ.2.OR.KMAL.EQ.1) GO TO 160
            RVE=SQRT(VTX2(1)**2+VTX2(2)**2)
            CALL YVDCOF(IXHX(1),NUMC1,XYZCO,RADC1,FIICO,ZETCO,NCOM1,
     &        LDMP1)
            CALL YVDCOF(IXHX(2),NUMC2,XYZCO,RADC2,FIICO,ZETCO,NCOM2,
     &        LDMP1)
C IF BOTH TRACKS HAVE 2 COMPLETE HITS CONTINUE IN ANY CASE
            IF(NCOM1.GE.2.AND.NCOM2.GE.2) GO TO 59
C OTHERWISE, IF THE VDET HITS ARE IN FRONT OF THE FOUND VTX IGNORE THEM
C     IF(RADC1(NUMC1).GT.RVE.AND.NUMC1.GT.0) GO TO 59
C     IF(RADC2(NUMC2).GT.RVE.AND.NUMC2.GT.0) GO TO 59
C     GO TO 161
C IF BOTH TRACKS HAVE NO HIT INFORMATION (MINI) REFIT IF RADIUS < 6 CM
            IF(NCOM1.EQ.-1.AND.NCOM2.EQ.-1) THEN
              IF(RVE.LE.6.) GO TO 59
              GO TO 61
            ENDIF
C OTHERWISE, IF THE VDET HITS ARE IN FRONT OF THE FOUND VTX IGNORE THEM
            IF(NUMC1.GT.0 .AND. RADC1(1).LT.RVE) GOTO 61
            IF(NUMC2.GT.0 .AND. RADC2(1).LT.RVE) GOTO 61
            IF(NUMC1.EQ.0 .AND. NUMC2.EQ.0) GO TO 61
   59       CONTINUE
C REFIT INCLUDING VDET HITS
            KFRFTU=KFRFT
            KOFRF=2
  160     CONTINUE
  161     CONTINUE
          IF(IFAIL.NE.0.OR.CHVX2.GT.CHVXCO) GO TO 60
C
C  REQUIRE VERTEX OUTSIDE VACUUM
          RVX=SQRT(VTX2(1)**2+VTX2(2)**2)
          IF(RVX.LT.RVACCO) GOTO 60
C
          NPIDC=1
          AMPC(1)=YPMASS(JPAFEP)
          AMPC(2)=YPMASS(JPAFEM)
C
C     conversion vertex complete fit
          CALL YFMVTR(0,2,0,.FALSE.,.TRUE.,.TRUE.,.TRUE.,
     &      VTX2,VARV2,IXHX,
     &      LCOLS(KFRFT),LVHXIN,
     &      RW(KFRFT+LMHLEN+JFRFIR),VHXIN(1,1,KOFRF),
     &      IXNU,
     &      NSNU,NSVNU,TNUI,VTNUI,
     &      NPIDC,AMPC,
     &      VXOUT,VVXOU,HXOU,VHXOU,TNUO,VTNUO,
     &      PSUM,VPSUM,VPSVX,
     &      AMASS,DMASS,VMVX,VMPS,
     &      CHISQ,IFAIL)
C
C  FIRST CHECK CHISQ AGAIN
          IF(IFAIL.NE.0.OR.CHISQ.GT.CHVXCO) GO TO 60
C
          P1=ABS(PRECTO(IXHX(1)))
          P2=ABS(PRECTO(IXHX(2)))
          POP=P1/P2+P2/P1
          AM02=YPMASS(JPAFEP)**2*(2.+POP)
          AMTH2=YPMASS(JPAFEP)**2*POP
C LIMIT DMASS TO 1 GEV
          DMASS=MIN(1.,DMASS)
C------ CALCULATED ERRORS ON THE MASS IN THE CASE OF PHOTON
C       CONVERSIONS ARE NOT REALLY CORRECT IN THE LINEAR
C       APPROXIMATION USED IN THE YTOP METHOD
C       IF((AMASS**2-AM02).GT.CHML*(AMTH2+DMASS**2)) ....
C       USE A FIX CUT ON THE MASS INSTEAD OF A CUT A CERTAIN
C       NUMBER OF SIGMAS
          IF(AMASS.GT.AMCTCO) THEN
            GO TO 60
          ELSE
C
            IF(LCONMC) THEN
C
C  MASS CONSTRAINED VERTEX FIT
              AMCON=AM02
              DMQCON=AMAX1(AMTH2,.0001)
              CALL YFVMC(0,2,0,.FALSE.,
     &          VTX2,VVXIN,IXHX,
     &          LCOLS(KFRFTU),LVHXIN,
     &          RW(KFRFTU+LMHLEN+JFRFIR),VHXIN(1,1,KOFRF),
     &          IXNU,
     &          NSNU,NSVNU,TNUI,VTNUI,
     &          AMPC(1),AMCON,DMQCON,
     &          VXOUT,VVXOU,HXOU,VHXOU,TNUO,VTNUO,
     &          PSUM,VPSUM,VPSVX,
     &          AMASS,DMASS,
     &          CHISQ,IFAIL)
C
C
              IF(IFAIL.NE.0.OR.CHISQ.GT.CHVXCO) GO TO 60
C
            ENDIF
C
            CALL YTPAR(0,VXOUT,VVXOU,PSUM,VPSUM,VPSVX,
     &        TRACK,VTRACK,IFAIL)
            IF(IFAIL.GT.0) GOTO 60
C
C POINTING CHISQ OF RECONSTRUCTED GAMMA W.R.T BCRO
            IXNU(1)=1
            CALL YFTVTR(1,0,1,.FALSE.,BCROSS,VBCROS,
     &        IDUM,IDUM,IDUM,
     &        DUM,DUM,
     &        IXNU,5,15,TRACK,VTRACK,VXOU2,VVXO2,CHIS2,IFAIL)
            IF(IFAIL.GT.0)GOTO 60
C-- CUT ON THE POINTING CHISQ (2 D.O.F.)
            IF(CHIS2.GT.CHPTCO) GOTO 60
C
C     vertex distance
            VDIST=SQRT((VXOUT(1)-VXOU2(1))**2
     &          +(VXOUT(2)-VXOU2(2))**2
     &          +(VXOUT(3)-VXOU2(3))**2)
C
C     vertex dist. chisq (refit decay tracks to handle nonlinear probl.)
            CALL YFTVTR(1,2,0,.FALSE.,BCROSS,VBCROS,IXHX,
     &        LCOLS(KFRFTU),LVHXIN,
     &        RW(KFRFTU+LMHLEN+JFRFIR),VHXIN(1,1,KOFRF),
     &        IDUM,IDUM,IDUM,DUM,DUM,WTX,VARWX,CHIVS,IFAIL)
C
C     REJECT CONV.WITH ADDITIONAL TRACKS PASSING THROUGH VERTEX
C
            NADD=0
            DO 70  KK=1,II
              KP=IPTR(KK)
              IF(IP.NE.KP.AND.JP.NE.KP) THEN
                CALL YFTVTR(1,1,0,.FALSE.,VTX2,VARV2,KP,
     &            LCOLS(KFRFTU),LVHXIN,
     &            RW(KFRFTU+LMHLEN+JFRFIR),VHXIN(1,1,KOFRF),
     &            IDUM,IDUM,IDUM,DUM,DUM,WTX,VARWX,CHISA,IFAIL)
                IF(CHISA.LT.10.     ) THEN
                  NADD=NADD+1
                  ITADD(NADD)=KP
                  IF(NADD.GT.NAMXCO) THEN
                    GO TO 60
                  ENDIF
                ENDIF
              ENDIF
   70       CONTINUE
C
C      set particle origin flag
            CALL YMKSET(1,NMSIZZ,KPORF0(IP),JPAFPH)
            CALL YMKSET(1,NMSIZZ,KPORF0(JP),JPAFPH)
C
C
C
C-- SAVE THE RECONSTRUCTED CONVERSION-VERTEX IN THE BANK PYER
            LGARB=.FALSE.
C----- output to BOS-BANK PYER
C           KPYER=IW(NAMIND('PYER'))
            KPYER=NLINK('PYER',0)
            IF(KPYER.GT.0) THEN
C  --- BANK ALREADY EXISTS
              KLAST = LROWS(KPYER)+1
            ELSE
              KLAST = 1
            ENDIF
            KYWI  = LPYERA*KLAST
C  --- WE BOOK HERE THE SPACE FOR THE BANK
            CALL AUBOS('PYER',0,LMHLEN+KYWI,KPYER,IRET)
C  --- ? NO SPACE
            IF(IRET.EQ.2) GOTO 997
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KPYER+LMHCOL) = LPYERA
            IW(KPYER+LMHROW) = KLAST
C  ---?
            IPYER = KROW(KPYER,KLAST)
C  --- STORE INFORMATION
C  --- TYPE OF VERTEX 0..255 1=MAIN 2=V0,3=MAIN FOR 2-PRONGS
C                                   4=CONVERSION
            IW(IPYER+JPYETY)      = 4
C-- copy the vertex position
            CALL UCOPY(VXOUT(1),RW(IPYER+JPYEVX),3)
C-- copy the variances
C----- covariance matrix 1 2 4
C                          3 5
C                            6
            CALL UCOPY(VVXOU(1),RW(IPYER+JPYEVM),6)
C-- copy the chisq
C----- C2 Chisquare 0.0 ...255.
            RW(IPYER+JPYEC2) = CHISQ
C-- copy the number of degrees of freedom,
C                    2x2 for each track - 3 for vertex constraint
            IW(IPYER+JPYEDF) = 1

C-- save the track indices belonging to the secondary vertex
C-- in the bank PYFR
            KPYFR=IW(NAMIND('PYFR'))
            IF(KPYFR.GT.0) THEN
C----- bank already exists
              NRPYF = LROWS(KPYFR)+2
            ELSE
              NRPYF = 2
            ENDIF
            CALL AUBOS('PYFR',0,LMHLEN+LPYFRA*NRPYF,KPYFR,IRET)
            IF(IRET.EQ.2) GOTO 996
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KPYFR+LMHCOL) = LPYFRA
            IW(KPYFR+LMHROW) = NRPYF
            DO 300 ITR = 1,2
              IPYFR = KROW(KPYFR,ITR+NRPYF-2)
C  --- vertex number
              IW(IPYFR+JPYFVN) = KLAST
C  --- track number
              IW(IPYFR+JPYFTN) = IXHX(ITR)
  300       CONTINUE
C
C-- save the incoming photon-track in the bank ynft
C      output to bos-bank pyer
            KYNFT=IW(NAMIND('YNFT'))
            IF(KYNFT.GT.0) THEN
C      bank already exists
              KLAST = LROWS(KYNFT)+1
            ELSE
              KLAST = 1
            ENDIF
            KYWI  = LYNFTA*KLAST
C
C  INDEX OF NEUTRAL TRACK
            INU=KLAST
C  SET PARTICLE IDENTIFICATION FLAG
            JP=INU+MAXHLX
            CALL YMKORR(1,NMSIZZ,KPIDF0(JP),JPAFPH,KPIDF0(JP))
C
C      we book here the space for the bank
            CALL AUBOS('YNFT',0,LMHLEN+KYWI,KYNFT,IRET)
C      ? no space
            IF(IRET.EQ.2) GOTO 995
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KYNFT+LMHCOL) = LYNFTA
            IW(KYNFT+LMHROW) = KLAST
C
            IYNFT = KROW(KYNFT,KLAST)
C      store information
C-- copy the track parameters
            CALL UCOPY(TRACK(1),RW(IYNFT+JYNFMO),5)
C-- copy the variances
C      covariance matrix 1 2 4 7 11
C                          3 5 8 12
C                            6 9 13
C                             10 14
C                                15
            CALL UCOPY(VTRACK(1),RW(IYNFT+JYNFEM),15)
C-- copy the chisq
C      C2 Chisquare
            RW(IYNFT+JYNFC2) = CHISQ
C-- copy the # of deg. of freedom, 2 x ntr - 3 (+1 for mass constraint)
            IW(IYNFT+JYNFDF) = 1
            IF(LCONMC) IW(IYNFT+JYNFDF) = 2
C
C-- charge
            IW(IYNFT+JYNFCH) = 0
C
C-- number of daughter tracks
            IW(IYNFT+JYNFND) = 2
C-- number of mass assignements
            IW(IYNFT+JYNFNM) = 1
C
C-- fill daughter track bank  YNTR
            KYNTR=IW(NAMIND('YNTR'))
            IF(KYNTR.GT.0) THEN
C      bank already exists
              KLAST = LROWS(KYNTR)+2
            ELSE
              KLAST = 2
            ENDIF
            KYWI  = LYNTRA*KLAST
C      we book here the space for the bank
            CALL AUBOS('YNTR',0,LMHLEN+KYWI,KYNTR,IRET)
C      ? no space
            IF(IRET.EQ.2) GOTO 995
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KYNTR+LMHCOL) = LYNTRA
            IW(KYNTR+LMHROW) = KLAST
C
            IYNTR=KROW(KYNTR,KLAST-1)
C      store information
            DO 330 ITR=1,2
C-- save the mother track number
              IW(IYNTR+LYNTRA*(ITR-1)+JYNTMT) = LROWS(KYNFT)
C-- save the daughter track number
              IW(IYNTR+LYNTRA*(ITR-1)+JYNTDT) = IXHX(ITR)
C-- save the daughter track type
              IW(IYNTR+LYNTRA*(ITR-1)+JYNTTT) = 1
  330       CONTINUE
C-- save the mass assign to the daughter using ALEPH particle table
C-- positive particle comes in first position
            IW(IYNTR+JYNTMA)= 2
            IW(IYNTR+LYNTRA+JYNTMA) = 3
C
C-- fill  mass assignment bank  YNMA
            KYNMA=IW(NAMIND('YNMA'))
            IF(KYNMA.GT.0) THEN
C      bank already exists
              KLAST = LROWS(KYNMA)+1
            ELSE
              KLAST = 1
            ENDIF
            KYWI  = LYNMAA*KLAST
C      we book here the space for the bank
            CALL AUBOS('YNMA',0,LMHLEN+KYWI,KYNMA,IRET)
C      ? no space
            IF(IRET.EQ.2) GOTO 995
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KYNMA+LMHCOL) = LYNMAA
            IW(KYNMA+LMHROW) = KLAST
C
            IYNMA=KROW(KYNMA,KLAST)
C      store information
C-- save the mother track number
            IW(IYNMA+JYNMMT) = LROWS(KYNFT)
C-- save the particle assignment : gamma
            IW(IYNMA+JYNMPA) = 1
C-- save the mass
            RW(IYNMA+JYNMMA) = AMASS
C-- save the error on the mass
            RW(IYNMA+JYNMEM) = DMASS
C-- save the vertex distance
            RW(IYNMA+JYNMVD) = VDIST
C-- save the chisq vertex separation
            RW(IYNMA+JYNMSC) = CHIVS
C-- save the impact parameter chisq
            RW(IYNMA+JYNMIC) = CHIS2
C-- save the numb. of add. tracks in conv. vertex
            IW(IYNMA+JYNMNA) = NADD
C-- save the electron probabilities
            RW(IYNMA+JYNMPR) = ELPR1
            RW(IYNMA+JYNMPR+1) = ELPR2
C
C-- fill 3-momentum bank  YNPE
            KYNPE=IW(NAMIND('YNPE'))
            IF(KYNPE.GT.0) THEN
C      bank already exists
              KLAST = LROWS(KYNPE)+1
            ELSE
              KLAST = 1
            ENDIF
            KYWI  = LYNPEA*KLAST
C      we book here the space for the bank
            CALL AUBOS('YNPE',0,LMHLEN+KYWI,KYNPE,IRET)
C      ? no space
            IF(IRET.EQ.2) GOTO 995
            IF(IRET.EQ.1) LGARB=.TRUE.
            IW(KYNPE+LMHCOL) = LYNPEA
            IW(KYNPE+LMHROW) = KLAST
C
            IYNPE=KROW(KYNPE,KLAST)
C      store information
            CALL UCOPY(PSUM,RW(IYNPE+JYNPPX),3)
C
C
C-- save relative pointers in between banks
C-- watch for garbage collection
            IF(LGARB) THEN
C       KPYER=IW(NAMIND('PYER'))
              KPYER=NLINK('PYER',0)
              KYNFT=IW(NAMIND('YNFT'))
              KYNTR=IW(NAMIND('YNTR'))
              KYNMA=IW(NAMIND('YNMA'))
            ENDIF
            LPYER=LROWS(KPYER)
            IPYER=KROW(KPYER,LPYER)
            LYNFT=LROWS(KYNFT)
            IYNFT=KROW(KYNFT,LYNFT)
            LYNMA=LROWS(KYNMA)
            IYNMA=KROW(KYNMA,LYNMA)
            LYNTR=LROWS(KYNTR)
            IYNTR=KROW(KYNTR,LYNTR-1)
            IW(IYNFT+JYNFPV)=LPYER
            IW(IYNFT+JYNFPM)=LYNMA
            IW(IYNFT+JYNFPT)=LYNTR-1
            DO 351 ITR=1,2
  351       IW(IYNTR+LYNTRA*(ITR-1)+JYNTMT)=LYNFT
C  --- end of bank output
C
          ENDIF
C
C--   fill the summary information
          NRCOTO = NRCOTO + 1
          PPS =  SQRT(PSUM(1)**2+PSUM(2)**2+PSUM(3)**2)
          AMCOTO = AMCOTO + PPS
          ACCOTO = ACCOTO + CHISQ
          ARCOTO = ARCOTO + SQRT(VXOUT(1)**2+VXOUT(2)**2)
C
   60 CONTINUE
   61 CONTINUE
C
      RETURN
C
  995 CALL ALTELL('YTCONV :  no space to create bank YNFT IER=1',0,
     &   ' RETURN ')
      GOTO 1000
  996 CALL ALTELL('YTCONV :  no space to create bank PYFR IER=1',0,
     &   ' RETURN ')
      GOTO 1000
  997 CALL ALTELL('YTCONV :  no space to create bank PYER IER=1',0,
     &   ' RETURN ')
      GOTO 1000
  998 CALL ALTELL('YTCONV: problem in UMSERR IER=1',0,' RETURN ')
      GOTO 1000
  999 CONTINUE
 1000 IER=1
      RETURN
      END
