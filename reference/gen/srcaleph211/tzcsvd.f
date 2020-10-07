      SUBROUTINE TZCSVD(ISLOT,R,PHI,Z,ZC)
C
C-----------------------------------------------------------------------
C! Correct TPC Coordinates for effects from sector missalignment
C! effects in Rz. This is an effective correction, because
C! only offsets of the sector centre relative to its nominal
C! position and tilts of the sector relative to the radial direction
C! from the origin to the nominal sector centre around the
C! nominal sector centre are considered.
C!
C!  Author    :   W. Wiedenmann  91/10/15
C!
CKEY TPC Z-CORRECTION VDET
C!
C!  Input     :
C!                ISLOT /I  : sector number
C!                R     /R  : radius of TPC coordinate  [cm]
C!                PHI   /R  : angle  of TPC coordinate  [radian]
C!                Z     /R  : z of TPC coordinate [cm]
C!
C!  Output     :  ZC   /R  : corrected Z coordinate
C!
C-----------------------------------------------------------------------
      SAVE
C
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JTRZSB=1,JTRZDS=2,JTRZRS=3,JTRZAS=4,LTRZSA=4)
      PARAMETER(JTCGID=1,JTCGVR=2,JTCGCN=4,JTCGNS=5,JTCGNC=6,JTCGNE=7,
     +          JTCGAS=8,JTCGRS=9,JTCGPH=10,JTCGPS=11,JTCGPW=12,
     +          JTCGGW=13,JTCGEW=14,JTCGES=15,JTCGBD=16,JTCGTS=17,
     +          JTCGTH=18,JTCGWP=19,JTCGWD=20,JTCGTO=21,JTCGTT=24,
     +          JTCGWT=27,JTCGWE=28,JTCGWW=29,JTCGWK=30,JTCGFT=33,
     +          LTCGDA=33)
      PARAMETER(JTCROC=1,JTCRNC=2,JTCRN1=3,LTCRLA=3)
      PARAMETER(JTMTID=1,JTMTVR=2,JTMTMO=4,JTMTPP=5,JTMTRF=6,JTMTNP=7,
     +          JTMTPR=8,JTMTRT=20,JTMTNT=21,JTMTTR=22,JTMTAT=33,
     +          JTMTTC=34,JTMTPW=38,JTMTNW=39,JTMTEF=40,JTMTEL=41,
     +          JTMTWF=42,LTMTYA=45)
      PARAMETER(JTPCIN=1,JTPCRV=2,JTPCPH=3,JTPCZV=4,JTPCSR=5,JTPCSZ=6,
     +          JTPCOF=7,JTPCTN=8,JTPCCN=9,JTPCIT=10,JTPCRR=11,
     +          JTPCRZ=12,LTPCOA=12)
      PARAMETER(JTPHIT=1,LTPHEA=1)
      PARAMETER(JTPHKT=1,JTPHCI=2,JTPHPH=3,JTPHZV=4,JTPHDD=5,JTPHDZ=6,
     +          LTPHTA=6)
      PARAMETER(JTPTKT=1,JTPTXA=2,JTPTYA=3,JTPTZA=4,JTPTDX=5,JTPTDY=6,
     +          JTPTDZ=7,JTPTPV=8,JTPTLN=9,JTPTTF=10,JTPTPM=11,
     +          JTPTCH=12,LTPTEA=12)
      PARAMETER(JTSGID=1,JTSGVR=2,JTSGNC=4,JTSGXC=5,JTSGYC=10,JTSGTM=15,
     +          LTSGMA=15)
      PARAMETER(JTSLID=1,JTSLVR=2,JTSLSN=4,JTSLSB=5,JTSLSS=6,JTSLDS=7,
     +          JTSLAS=8,JTSLRS=9,JTSLTM=10,JTSLTS=11,LTSLOA=11)
      PARAMETER(JTTHIT=1,LTTHEA=1)
      PARAMETER(JTTHKT=1,JTTHCI=2,JTTHPH=3,JTTHZV=4,JTTHDD=5,JTTHDZ=6,
     +          LTTHTA=6)
      PARAMETER(JTSCID=1,JTSCVR=2,JTSCSN=4,JTSCNS=5,JTSCRP=6,JTSCAZ=9,
     +          JTSCAX=10,JTSCAY=11,JTSCSG=12,JTSCTC=13,LTSCOA=13)
      PARAMETER(JT1FID=1,JT1FVR=2,JT1FLL=4,JT1FUL=5,JT1FSS=6,JT1FNP=7,
     +          LT1FCA=7)
      PARAMETER(JT2FID=1,JT2FVR=2,JT2FR1=4,JT2FR2=916,JT2FP1=1828,
     +          JT2FP2=2740)
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
C
C++   Correction coefficients for each sector
C
      REAL TANTHS(LTSECT), DELTZS(LTSECT)
      REAL RADSEC(LTSECT)
      LOGICAL FIRST
      INTEGER AGETDB
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
C
      ZC = Z
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTRZS=NAMIND('TRZS')
        NEVEH=NAMIND('EVEH')
        IRLST=0
        KTMTY=IW(NAMIND('TMTY'))
        KTCGD=IW(NAMIND('TCGD'))
        RSTEP=RTABL(KTCGD,1,JTCGRS)
        DO 5 LS=1,LTSECT
           ISTYP = ITPTYP(LS)
           NPADR= ITABL(KTMTY,ISTYP,JTMTNP)
           RFST= RTABL(KTMTY,ISTYP,JTMTRF)
           RLST= RFST + FLOAT(NPADR-1)*RSTEP
           RADSEC(LS) = 0.5*(RFST+RLST)
5       CONTINUE
      ENDIF
C
C++   Get the current run number
C
      KEVEH=IW(NEVEH)
      IF (KEVEH.EQ.0) THEN
        CALL ALTELL('TZCSVD: Cannot find eveh bank to get the'//
     &              ' run number.',0,'RETURN')
        IRUN=4001
      ELSE
        IRUN=IW(KEVEH+JEVERN)
      ENDIF
C
C++   Link to the TPC z correction bank
C
      IF (IRUN.LE.2000) THEN
        RETURN
      ELSE
        IF (IRLST.NE.IRUN) THEN
          IRET=AGETDB('TRZS',IRUN)
          IF (IRET.EQ.0) THEN
            CALL ALTELL('TZCSVD: '//
     &       'Bank TRZS is missing from the database.',0,'RETURN')
          ENDIF
          KTRZS=IW(NTRZS)
          IF (KTRZS.NE.0) THEN
            DO 10 LS=1,LTSECT
               DELTZS(LS) =      RTABL(KTRZS,LS,JTRZDS)
               TANTHS(LS) = TAN( RTABL(KTRZS,LS,JTRZRS) )
10          CONTINUE
          ELSE
            DO 20 LS=1,LTSECT
               DELTZS(LS) = 0.
               TANTHS(LS) = 0.
20          CONTINUE
          ENDIF
          IRLST=IRUN
        ENDIF
      ENDIF
C
C++   Transform to sector coordinate system
C
      CALL TCTSEC(ISLOT,R,PHI,Z,RS,RPHIS,ZS)
      PHIS = RPHIS/RS
      IF (PHIS.GT.PI) THEN
        PHIS=PHIS-TWOPI
      ELSEIF (PHIS.LT.-PI) THEN
        PHIS=PHIS+TWOPI
      ENDIF
C
C++   Calculate effective z-shift
C
      DZ  = (RS*COS(PHIS)-RADSEC(ISLOT))*TANTHS(ISLOT) + DELTZS(ISLOT)
      ZCS = ZS - DZ
C
C++   Transform to global coordinate system
C
      CALL TCTGLB(ISLOT,RS,RPHIS,ZCS,RG,RPHIG,ZG)
      PHIG = RPHIG/RS
      IF (PHIG.GT.TWOPI) THEN
        PHIG=PHIG-TWOPI
      ELSEIF (PHIG.LT.0.) THEN
        PHIG=PHIG+TWOPI
      ENDIF
C
      ZC = ZG
C
      RETURN
      END
