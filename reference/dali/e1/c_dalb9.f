C*DK SIIDPO
      SUBROUTINE SIIDPO(IMD,IST,IPH,IRD,ZZ,PHI,RAD)
C.---------------------------------------------------------------------
CKEY SCALDES DECODE / USER
C     B.BLOCH     December 1993
C! Transform subcomponents indices into Z,phi,rho coordinates
C! without fine alignment = ideal positions
C   Input :
C          IMD   Module number ( 1-2)
C          IST   Z stack number ( 1-12)
C          IPH   Phi bin number ( 1-32)
C          IRD   Radial bin number ( 1-16)
C   Output:
C          ZZ,PHI,RAD    corresponding z, phi ( degrees), rho
C          RAD = -1.     means error
C   Called by USER program
C.---------------------------------------------------------------------
C*IF .NOT.DOC
      SAVE
      INCLUDE 'A_BCS.INC'
C*CA ALCONS
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
C*IF DOC
C*EI
C*CC ALCONS
C*CA SIGECO
      COMMON/SIGECO/NMODSI,NRBNSI,NPBNSI,NZBNSI,RMINSI(2),RMAXSI(2),
     $              Z0SNSI(2),ZWIDSI,ZWRFSI,ZWRLSI,ZWFRSI,ZWFLSI,
     $              ZWBKSI,ZWLASI,OVLPSI,DPOSSI(3,2),GAPXSI(2),
     $              PHSHFT(3,2),RADSTP,PHISTP,ISINUM(12,2)
C*IF DOC
C*EI
C*CC SIGECO
C*CA BMACRO
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
C*IF ETA
C*EI
C*CC BMACRO
      RAD =-1.
      IOR = MOD(IST-1,3)
C GET RADIUS
      RAD = RMINSI(IMD)+ RADSTP*(IRD-1)+0.5*RADSTP
C GET PHI
      ITP = IOR +1
      PHI = PHISTP*(IPH-1) + PHSHFT(ITP,IMD)+0.5*PHISTP
C GET Z
      ZED = Z0SNSI(IMD)+ ZWIDSI*(IST-1)
      IF ( IMD.EQ.2 ) ZED = -ZED
C
      PHI = PHI*RADEG
      ZZ = ZED
 998  RETURN
      END



      SUBROUTINE DFTRAK
C-------------------------------------------------------------------
C
C!  extrapolate TPC track through Ecal, Coil, Hcal
C!
C!  G.Capon                              28-Aug-87
C!
C!
C!  -----------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
C*CA HCALG1
      COMMON/HCALG1/HT0LAY,HTSLAB,HCTGAP,HXBMIN,HXBMAX,
     +  HREMIN,HREMAX,HREMED,HZEMIN,HZBMIN,HTSAMP,HTHCLE,
     +  HYBMIN,HYBMAX,HZEMAX,HZBMAX
C*IF DOC
C*CC HCALG1
C*CA ALCONS
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
C*IF1 CRAY.OR.ETA
C*EL1
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C*EI1
C
C*IF DOC
C*CC ALCONS
C*CA RPARAC,RFLAGS
      PARAMETER (JULVD=1,JULIT=2,JULTP=3,JULEC=4,JULLC=5)
      PARAMETER (JULSA=6,JULHC=7,JULMU=8,JULCA=9,JULSK=10)
      PARAMETER (JULYR=11,JULYT=12,JULEF=13,JULBC=14,JULFA=15
     +           ,JULBO=16)
      PARAMETER (MODEND=0,MODREA=1,MODPRV=2,MODPRI=3,MODPRT=4)
      PARAMETER (MODPRE=5,MODPRL=6,MODPRS=7,MODPRH=8,MODPRM=9)
      PARAMETER (MODTPT=10,MODTPW=11,MODDEX=12,MODITC=13 ,MODVDE=14)
      PARAMETER (MODFIT=15,MODFTR=16,MODECL=17,MODMIP=18,MODEID=19)
      PARAMETER (MODFEO=20,MODFEP=21,MODHCL=22,MODHDP=23,MODEHG=24)
      PARAMETER (MODMUF=25,MODMUA=26,MODLCA=27,MODSAT=28,MODLTR=29)
      PARAMETER (MODPID=30,MODYMA=31,MODYTO=32,MODYV0=33,MODHIS=34)
      PARAMETER (MODPOT=35,MODMIN=36,MODWRI=37)
      PARAMETER (MODCOM=38,MODBCA=39,MODBOO=40,MODMON=41,MODENF=42)
      PARAMETER (MODGAM=43,MODEDI=44,MODGMX=45)
      PARAMETER (MODSUM=46)
      PARAMETER (LASTMO=46)
C
C
C-------> PARAMETERS GLOBAL OVER PROGRAM
C
C SKELETON Parameters
C -------------------
C JULVD   =  1, VDET identifier
C JULIT   =  2, ITC  identifier
C JULTP   =  3, TPC  identifier
C JULEC   =  4, ECAL identifier
C JULLC   =  5, LCAL identifier
C JULSA   =  6, SATR identifier
C JULHC   =  7, HCAL identifier
C JULMU   =  8, MUON identifier
C JULCA   =  9, CALO identifier
C JULSK   = 10, SKEL identifier
C JULYR   = 11, YREC identifier
C JULYT   = 12, YTOP identifier
C JULEF   = 13, EFLO identifier
C JULBC   = 14, BCAL identifier
C
C MODULE identifiers USED IN RNXMOD
C ---------------------------------
C MODEND=0   NOMODULE -used to close a module
C MODREA=1   Reading records                  - RDOJOB
C MODPRV=2   Prepare data VDET                - VPREDA
C MODPRI=3   Prepare data ITC                 - IPREDA
C MODPRT=4   Prepare data TPC                 - TPREDA
C MODPRE=5   Prepare data ECAL                - EPREDA
C MODPRL=6   Prepare data LCAL                - LPREDA
C MODPRS=7   Prepare data SATR                - SPREDA
C MODPRH=8   Prepare data HCAL                - HPREDA
C MODPRM=9   Prepare data MUON                - MPREDA
C MODTPT=10  TPC track finding                - TPCREC (first part)
C MODTPW=11  TPC wire association             - TRKWIR
C MODDEX=12  dE/dx calculation                - TRKELS
C MODITC=13  ITC reconstruction               - ITCREC
C MODVDE=14  VDET reconstruction              - VDLINK
C MODFIT=15  Global fit VDET,ITC,TPC          - FITALL
C MODFTR=16  Extrapolate track ECAL,Coil,Hcal - FTRACK
C MODECL=17  Find ECAL clusters               - ECFCLU
C MODMIP=18  Minimum ionizing in ECAL         - ECRMIP
C MODEID=19  Electron identification          - ELECID
C MODFEO=20  ECAL cluster analysis            - ECFOBJ
C MODFEP=21  ECAL statistics                  - ECPRIN
C MODHCL=22  Find HCAL clusters               - HCFCLU
C MODHDP=23  HCAL digital pattern analysis    - HDPREC
C MODEHG=24  ECAL - HCAL global analysis      - ECHCGL
C MODMUF=25  HCAL muon finding                - HMFIND
C MODMUA=26  MUON-hit - track association     - MUASS
C MODLCA=27  LCAL reconstruction              - LCAREC
C MODSAT=28  SATR reconstruction              - SATREC
C MODLTR=29  Luminosity track-cluster assoc.  - LTRACK
C MODPID=30  Particle identification summary  - FPIDEN
C MODYMA=31  Main vertex finding: Mermekides  - YFMAIN
C MODYTO=32  Topology reconstruction          - YTOPOL
C MODYV0=33  V0 reconstruction                - YMFV0S
C MODHIS=34  Histogramming                    - RHSEVT
C MODPOT=35  Build pot banks                  - RPRPOT
C MODMIN=36  Build mini banks                 - RDEFMI
C MODWRI=37  Write event                      - RWREVT
C MODCOM=38  Data compression                 - CMPLIS
C MODBCA=39  BCAL reconstruction              - BPREDA
C MODBOO=40  Bookkeeping                      -
C MODMON=41  Monitoring                       - RMONIT
C MODENF=42  Energy Flow                      - EMSKEV
C MODGAM=43  ECAL Photon identification       - EBEGID
C MODEDI=44  EDIR Class Word                  - REDIRC
C MODEDI=45  Gammas in charged clusters       - ECGMIX
C MODSUM=46  Summary File                     - RCJSUM
C LASTMO=46  Largest module number
*EI
      INTEGER JDBDRF,JHISRF,JPRSRF,JPRERF,MDET
      LOGICAL FDEBRF,FDETRF
      PARAMETER (MDET=16)
      COMMON /RFLAGS/FDEBRF,FDETRF(MDET),JDBDRF(MDET),JHISRF(MDET),
     +        JPRSRF(MDET),JPRERF(MDET),JCMORF,JEBIRF(2),JFBIRF(2)
C*IF DOC
C*CC RPARAC,RFLAGS
C*CA TREXJJ
      PARAMETER(JTREX3=1,JTREP3=4,JTRERM=7,JTRERG=8,JTRELA=9,LTREXA=9)
C*CC TREXJJ
C*CA FRFTJJ
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
C*CC FRFTJJ
C*CA RCONDS
      COMMON /RCONDS/ FIELRC,ENERRC
C*IF DOC
C*CC RCONDS
C*CA ETP1JJ
      PARAMETER(JETPX3=1,JETPP3=4,JETPPT=7,JETPCH=8,JETPEF=9,LETP1A=9)
C*CC ETP1JJ
      PARAMETER (NSTEP=26)
C                        parametrisation of dE/dx in Ecal and Coil
      PARAMETER (AL1=.172E-3,AL2=-8.2,AL3=1.14)
      PARAMETER (PB1=.166E-3,PB2=-6.1,PB3=4.12)
      PARAMETER (WMU=.10566, WMU2=.011164)
      DIMENSION ATFRA(9,NSTEP+4)
      DIMENSION DVE(7),DVF(7),ATIN(8),VV0(6)
      DIMENSION SCOO(NSTEP+4),STEP(NSTEP+4),TH2PR(NSTEP+4),R2MS(NSTEP+4)
      DIMENSION RADL(3)
      DATA RADL /2.4,8.05,2.45/
      DATA R1ECA,Z1ECA,R2ECA,Z2ECA,R1COI,R2COI
     +   / 185. , 245., 234., 297., 248.,264.7/
      DATA PCUT/1./
      DATA INIJO,NITER/0,4/
C*CA BMACRO
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
C*CC BMACRO
C
      IF(INIJO.EQ.0) THEN
        INIJO=1
        IBON=1
        IF(FIELRC .LT. 0.1) IBON=0
        CALL BLIST(IW,'T+','TREX')
C          Test presence of Hcal geometry banks :
C          if only TPC+ECAL processing is required one must call
C          Hiniru to have Hcal geometry (G.Capon - Feb 1990)
        JHBGE=IW(NAMIND('HBGE'))
        IF (JHBGE.EQ.0) CALL HINIRU
      ENDIF
      IF(FDETRF(JULEC)) CALL ECTPC(IER)
CC      IF(FDETRF(JULTP))THEN
CC         NFRFT=NAMIND('FRFT')
CC         JFRFT=IW(NFRFT)
CC         IF(JFRFT.EQ.0) GO TO 99
CC         NTRKS=LROWS(JFRFT)
CC      ELSE
CC C                        if FRFT missing, use ETP1
CC         IF(IER.NE.0) GO TO 99
CC         KETP1=NLINK('ETP1',0)
CC         NTRKS=LROWS(KETP1)
CC      ENDIF
C                     Drop previous TREX bank
      NTREX=NAMIND('TREX')
      JFRFT=IW(NTREX)
      IF(JTREX.NE.0) CALL BDROP(IW,'TREX')
C
      NFRFT=NAMIND('FRFT')
      JFRFT=IW(NFRFT)
      IF(JFRFT.EQ.0) GO TO 99
      NTRKS=LROWS(JFRFT)
C
C
C             loop on TPC tracks
C
      DO 10 ITRAC = 1,NTRKS
        NVETT=0
C-      Initialize ATFRA
        DO 100 I1 =  1,  9
          DO 110 I2 =  1,  NSTEP+4
            ATFRA (I1,I2) = 0.
  110     CONTINUE
  100   CONTINUE
CC    IF(FDETRF(JULTP))THEN
        CURV =RTABL(JFRFT,ITRAC,JFRFIR)
        TADIP=RTABL(JFRFT,ITRAC,JFRFTL)
        PHI  =RTABL(JFRFT,ITRAC,JFRFP0)
        DW   =RTABL(JFRFT,ITRAC,JFRFD0)
        ZW   =RTABL(JFRFT,ITRAC,JFRFZ0)
        VV0(1)=CURV
        VV0(2)=TADIP
        VV0(3)=PHI
        VV0(4)=DW
        VV0(5)=ZW
        VV0(6)=RTABL(JFRFT,ITRAC,JFRFAL)
C     get DW, PHI after mul. scatt. in ITC-TPC wall  !!
        CALL UNEWDP(VV0,DW,PHI)
C
        RHO=1./ABS(CURV)
        PT=FIELRC*RHO*CLGHT/100000.
        THETA = ATAN2 (1.,TADIP)
        CPHI  = COS(PHI)
        SPHI  = SIN(PHI)
        CTHET = COS(THETA)
        STHET = SIN(THETA)
C
        DVF(1) =+DW*SPHI
        DVF(2) =-DW*CPHI
        DVF(3) = ZW
        DVF(4) = STHET*CPHI
        DVF(5) = STHET*SPHI
        DVF(6) = CTHET
        PPINI  = PT/STHET
        DVF(7) = PPINI
        Q     =-SIGN (1.,CURV)
C
C?             select penetrating particles
C?             note that we have different p-cuts in FTRACK and MUASS
C
        IF (PPINI . LT. PCUT) GO TO 10
C
C             track extrapol up to Ecal entrance
C
        CALL AUHCYL(R1ECA,Z1ECA,FIELRC,Q,DVF,DVE,ICODE)
        IF(ICODE.EQ.0) GO TO 10
CC      ELSE
CC C             use Etp1 data
CC         IF(ITABL(KETP1,ITRAC,JETPEF).EQ.0)GO TO 10
CC         DO 11 ICOL=1,7
CC         DVE(ICOL)=RTABL(KETP1,ITRAC,ICOL)
CC   11    CONTINUE
CC         Q     = RTABL(KETP1,ITRAC,JETPCH)
CC         PPINI=DVE(7)
CC         DVF(7)=DVE(7)
CC         IF (PPINI . LT. PCUT) GO TO 10
CC      ENDIF
        NVETT=NVETT+1
        DVE(7)=DVF(7)
        ATFRA(8,NVETT)=1500.
        CALL UCOPY(DVE,ATFRA(1,NVETT),6)
        DO 21 K=4,6
   21   ATFRA(K,NVETT)=ATFRA(K,NVETT)*DVE(7)
C
C      track extrap through Ecal in NITER = 4 steps
C
        ENE=SQRT(DVF(7)*DVF(7)+WMU2)
        DO 25 NIT=1,NITER
          R2=R1ECA+(R2ECA-R1ECA)*FLOAT(NIT)/FLOAT(NITER)
          Z2=Z1ECA+(Z2ECA-Z1ECA)*FLOAT(NIT)/FLOAT(NITER)
          CALL AUHCYL(R2,Z2,FIELRC,Q,DVE,DVF,ICODE)
          IF(ICODE.EQ.0) GO TO 10
          DIST=VDIST(DVE,DVF,3)
          BE=DVF(7)/ENE
          GA=ENE/WMU
          DEDX=DIST*PB1*(ALOG(GA*BE)-BE*BE-PB2)*PB3/BE/BE
          ENE=ENE-DEDX
          IF(ENE .LT. 1.5*WMU) GO TO 51
          DVF(7)=SQRT(ENE*ENE-WMU2)
          IF(NIT.LT.NITER) CALL UCOPY(DVF,DVE,7)
   25   CONTINUE
        NVETT=NVETT+1
        ATFRA(8,NVETT)=0.
        CALL UCOPY(DVF,ATFRA(1,NVETT),6)
        DO 22 K=4,6
   22   ATFRA(K,NVETT)=ATFRA(K,NVETT)*DVF(7)
C
C             track extrapol through coil
C             approx Zcoil=Zhcal !!, R2coil=rmax-field !!
C
        CALL AUHCYL(R1COI,HZEMIN,FIELRC,Q,DVF,DVE,ICODE)
        IF(ICODE.EQ.1) THEN
          NVETT=NVETT+1
          DVE(7)=DVF(7)
          ATFRA(8,NVETT)=2500.
          CALL UCOPY(DVE,ATFRA(1,NVETT),6)
          DO 23 K=4,6
   23     ATFRA(K,NVETT)=ATFRA(K,NVETT)*DVE(7)
C
          CALL AUHCYL(R2COI,HZEMIN,FIELRC,Q,DVE,DVF,ICOD4)
          NVETT=NVETT+1
          ATFRA(8,NVETT)=0.
          DIST=VDIST(DVE,DVF,3)
C                   correct DIST for real coil thickness
          DIST=DIST*44./16.7
          BE=DVF(7)/ENE
          GA=ENE/WMU
          DEDX=DIST*AL1*(ALOG(GA*BE)-BE*BE-AL2)*AL3/BE/BE
          ENE=ENE-DEDX
          IF(ENE .LT. 1.5*WMU) GO TO 51
          DVF(7)=SQRT(ENE*ENE-WMU2)
          CALL UCOPY(DVF,ATFRA(1,NVETT),6)
          DO 24 K=4,6
   24     ATFRA(K,NVETT)=ATFRA(K,NVETT)*DVF(7)
        ELSE
          CALL UCOPY(DVE,ATIN,6)
          GO TO 40
        ENDIF
C
C              extrapolate track up to Hcal first plane
C
        IF(ICOD4.EQ.1)THEN
          PHI1=PIBY12
          CALL AULPOL(HXBMIN,HZEMIN,PHI1,DVF,ATIN,ICODE)
          IF(ICODE.EQ.0) GO TO 10
        ELSE
          CALL UCOPY(DVF,ATIN,6)
        ENDIF
C
C               follow track in Hcal (with mag field)
C
   40   CONTINUE
        ATIN(4)=ATIN(4)*DVF(7)
        ATIN(5)=ATIN(5)*DVF(7)
        ATIN(6)=ATIN(6)*DVF(7)
        ATIN(8)=Q
        CALL HTRACK(IBON,1,ATIN,IFAIL,NVETT,ATFRA)
C
C          create TREX bank (bank NR = ITRAC )
C
   51   CONTINUE
        LEN=2+LTREXA*NVETT
        CALL AUBOS('TREX',ITRAC,LEN,JTREX,IGARB)
        IF(IGARB.EQ.2) THEN
          CALL RERROR('FTRACK',1,'no space for TREX bank')
          GO TO 99
        ENDIF
        IW(JTREX+1)=LTREXA
        IW(JTREX+2)=NVETT
        KTREX=JTREX+2
C
C          fill TREX bank
C
        DO 50 N=1,NVETT
          CALL UCOPY(ATFRA(1,N),RW(KTREX+1),6)
          IW(KTREX+JTRERG)=NINT(ATFRA(8,N))
          IW(KTREX+JTRELA)=NINT(ATFRA(9,N))
          KTREX=KTREX+LTREXA
   50   CONTINUE
C
        SCOO(1)=0.
        CALL VZERO(R2MS,NSTEP+4)
        CALL VZERO (TH2PR,NSTEP+4)
C
C          compute steps and partial lengths of trajectory
C
        DO 60 N=1,NVETT-1
          DX=RTABL(JTREX,N+1,JTREX3)-RTABL(JTREX,N,JTREX3)
          DY=RTABL(JTREX,N+1,JTREX3+1)-RTABL(JTREX,N,JTREX3+1)
          DZ=RTABL(JTREX,N+1,JTREX3+2)-RTABL(JTREX,N,JTREX3+2)
          STEP(N)=SQRT(DX*DX+DY*DY+DZ*DZ)
          SCOO(N+1)=SCOO(N)+STEP(N)
          NMED=ITABL(JTREX,N,JTRERG)/1000
          IF(NMED.GT.0 .AND. NMED.LE.3) THEN
            PX=RTABL(JTREX,N,JTREP3)
            PY=RTABL(JTREX,N,JTREP3+1)
            PZ=RTABL(JTREX,N,JTREP3+2)
            PTOT2=PX*PX+PY*PY+PZ*PZ
            ETOT2=PTOT2+WMU2
            PIBE2=PTOT2*PTOT2/ETOT2
            CORR=1.
            CORR   =  (1.+ LOG10 (STEP(N)/RADL(NMED))/9.)**2
            TH2PR(N)=225.0E-6/PIBE2*STEP(N)/RADL(NMED)*CORR
          ENDIF
C
C                          compute average mult. scatt. radius
C
          DO 70 I=1,N
            DVOL=SCOO(N+1)-SCOO(I+1)
            R2MS(N+1)=R2MS(N+1) + TH2PR(I)*(STEP(I)*STEP(I)/3.+
     +        STEP(I)*DVOL+DVOL*DVOL)
   70     CONTINUE
C
          JROW=KROW(JTREX,N+1)
          RW(JROW+JTRERM)=SQRT(R2MS(N+1))
   60   CONTINUE
C
C              for first row put IFAIL in place of m.s. radius !!
C
        JROW=KROW(JTREX,1)
        RW(JROW+JTRERM)=IFAIL
C
C?             ...and now the next track
C
   10 CONTINUE
C
   99 RETURN
C
      END
C*DK HTRACK
      SUBROUTINE HTRACK(IBON,IDEON,ATIN,IFAIL,NVETT,ATFRA)
C-----------------------------------------------------------------
C
C! Full tracking routine for tracks in HCAL
C!
C! P.Campana - LNF - Version 3.2           121287
C!
C! GeV cm kG ns ALEPH coord. frame -
C! Parametriz. of the field - FFIELD called at entrance region
C!
C! IBON = 1 field active   IDEON = 1 dE/dx active
C! ATIN(8)  input  vector:  x y z px py pz Rms Q
C! ATFRA(9,NSTEP+4)  out :  x y z px py pz Rms #reg&mod #lay
C! Regions : 0=air, 1500=ecal, 2500=coil, 31xx-36xx=hcal
C! Regions in hcal x1xx=barrel     x2xx=notch     x3xx=back e.c.
C!                 x4xx=front e.c. x5xx=air zone  x6xx=coil
C! Mod.# for end caps refers to the mod.# of the barrel facing it
C! Negative sign refers to the z direction
C!
C! Tracking of particles with p>1 GeV entering HCAL, followed
C! up to pcut=0.15 GeV
C! IFAIL=1 ok, =2 p<pthr, =3 spiral, =4 p<pcut, =5 parall. to slabs
C!
C! With respect to 3.1: Variable end cap radius, Energy loss correct
C! in reg #6, Positive and *100 numbering of regions and modules
C-----------------------------------------------------------------
C
      PARAMETER (NSTEP=26)
      PARAMETER (ZBMED=348.,TG15=0.267949)
      PARAMETER (CRMIN=248.,CRMAX=292.,CZMAX=351.)
C
      PARAMETER (THRP=1.0,TBCUT=0.1,TPTCU=0.01)
      PARAMETER (WMU=.10566,WMU2=0.011164,W2MU=.21132)
      PARAMETER (UOM=8.98755E-3,URADI=3335.641)
      PARAMETER (UDFE=7.87,UDHC=0.706)
      PARAMETER (UDE1=.168E-3,UDE2=-7.8,UDE3=UDFE*UDHC)
      PARAMETER (CDE1=.171E-3,CDE2=-8.4,CDE3=1.14)
      PARAMETER (TR5=0.0872665,TA=1./TG15)
      PARAMETER (SI30=0.5,CO30=.8660254,TG30=0.57735)
C*CA ALCONS
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
C*IF1 CRAY.OR.ETA
C*EL1
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C*EI1
C
C*IF DOC
C*CC ALCONS
C*CA HCALG1
      COMMON/HCALG1/HT0LAY,HTSLAB,HCTGAP,HXBMIN,HXBMAX,
     +  HREMIN,HREMAX,HREMED,HZEMIN,HZBMIN,HTSAMP,HTHCLE,
     +  HYBMIN,HYBMAX,HZEMAX,HZBMAX
C*IF DOC
C*CC HCALG1
C
      DIMENSION ATIN(*),ATMOD(10,NSTEP+4),ATFRA(9,*)
      DIMENSION TTIN(6),TTOUT(6),VECOS(NSTEP+4),VESIN(NSTEP+4)
      DIMENSION VA(3,3),VRPR(3),VVPR(3),VPPR(3)
      DIMENSION POS(3),B(3)
      DATA INIBB/0/
C
C? Initialisations & module identification
C
      DO 705 K=1,NSTEP+4
        DO 705 J=1,10
  705 ATMOD(J,K)=0.
      STEP=HTSAMP
      NVETT=NVETT+1
      NVET1=NVETT
      NSLAB=1
      IFAIL=0
      ISIDE=1
      UBBT=0.
      TPP=0.
      OTLE=0.
      ULAST=1.
      TEMAX=HREMAX
C
      TALPH=ATAN2(ATIN(2),ATIN(1))
      IF(TALPH.LT.0.) TALPH=TALPH+TWOPI
      IMOD=TALPH/PIBY6+1.5
      IF(IMOD.GT.12) IMOD=IMOD-12
      TIMOD=IMOD
      TALPH=PIBY6*(TIMOD-1.)
      COSTA=COS(TALPH)
      SINTA=SIN(TALPH)
      VECOS(NVET1)=COSTA
      VESIN(NVET1)=SINTA
      ATMOD(7,NVET1)=TIMOD
      ATMOD(8,NVET1)=ATIN(7)
      ATMOD(10,NVET1)=1.
      TQ=ATIN(8)
      IBOLD=-99
      IF(INIBB.EQ.0) THEN
        CALL FMINIT
        INIBB=1
      ENDIF
C
C? Transformation from ALEPH coordinates to Y X Z frame and to module co
C
      DO 707 J=1,4,3
        TTIN(J)  =ATIN(J)*COSTA+ATIN(J+1)*SINTA
        TTIN(J+1)=ATIN(J)*SINTA-ATIN(J+1)*COSTA
  707 TTIN(J+2)=ATIN(J+2)
      DO 706 L=1,6
  706 ATMOD(L,NVET1)=TTIN(L)
      IF(TTIN(3).LT.0.) ISIDE=-1
C
C+ Identification of the region
C
      TRIN=SQRT(TTIN(1)*TTIN(1)+TTIN(2)*TTIN(2))
      TABS3=ABS(TTIN(3))
      IF((TTIN(1)).GE.(HXBMIN-.01)) THEN
        IF(TABS3.LE.ZBMED) THEN
          IREG=1
          GOTO 740
        ENDIF
        IF(TABS3.LT.HZBMAX) THEN
          IREG=2
        ELSE
          IREG=3
        ENDIF
        GOTO 740
      ENDIF
      IF(TABS3.GE.HZBMAX) THEN
        IREG=3
        GOTO 740
      ENDIF
      IF(TRIN.GT.HREMED) THEN
        IREG=5
      ELSE
        IREG=4
      ENDIF
      IF(TRIN.GT.CRMIN.AND.TRIN.LE.CRMAX.AND.
     +          TABS3.GE.(HZEMIN-0.01).AND.TABS3.LT.CZMAX) THEN
        IREG=6
      ENDIF
  740 ATMOD(9,NVET1)=IREG
C
C? Check on momentum & energy loss in the first pre-layer of the barrel
C
      TPEV=SQRT(TTIN(4)*TTIN(4)+TTIN(5)*TTIN(5)+TTIN(6)*TTIN(6))
      TEEV=SQRT(TPEV*TPEV+WMU2)
      TGEV=TEEV/WMU
      TBEV=TPEV/TEEV
      IF(TPEV.LT.THRP) THEN
        IFAIL=2
        GOTO 777
      ENDIF
      IF(IREG.LE.2) THEN
        TEEV=TEEV-HT0LAY*UDFE*
     +               (ALOG(TBEV*TGEV)-TBEV*TBEV-UDE2)*UDE1/TBEV/TBEV
        TPEV2=TEEV*TEEV-WMU2
        IF(TPEV2.LE.0) TPEV2=0.0001
        TPEV=SQRT(TPEV2)
      ENDIF
C
C? Starts loop over steps
C
    1 CONTINUE
C
C? Evaluates end-cap radius
C
      IF(IREG.EQ.3) THEN
        TPHI=ATAN2(TTIN(2),TTIN(1))
        TEMAX=HREMAX/ABS(COS(TPHI))
      ENDIF
C
C? Check if the angle track/slab is too small
C
      IF(IREG.LE.2) THEN
        TANG=ABS(ATAN2(TTIN(4),TTIN(6)))
      ELSE
        TANG=ABS(ATAN2(TTIN(6),TTIN(4)))
      ENDIF
      IF(TANG.LT.TR5) THEN
        IFAIL=5
        GOTO 777
      ENDIF
C
C? Kinematics
C
      TGEV=TEEV/WMU
      TBEV=TPEV/TEEV
      IROLD=IREG
      TPOLD=TPEV
      TDEN=1.
C
C? Last slab ?
C
      IF((NSLAB+1).GT.(NSTEP-2)) THEN
        IFAIL=1
        GOTO 777
      ENDIF
      STEPK=STEP
      IF(NSLAB.EQ.(NSTEP-3)) THEN
        STEPK=2.*HTSLAB
        ULAST=1./UDHC
      ENDIF
      DO 757 K=1,6
  757 TTOUT(K)=0.
C
C? Magnetic field evaluation in Aleph frame
C
      POS(1)=TTIN(1)*COSTA+TTIN(2)*SINTA
      POS(2)=TTIN(1)*SINTA-TTIN(2)*COSTA
      POS(3)=TTIN(3)
C                          increase by 1% to be sure to be in Hcal
      POS(1)=1.01*POS(1)
      POS(2)=1.01*POS(2)
      POS(3)=1.01*POS(3)
C
C? Get magnetic field region # IBREG (to be done at entrance point)
C
      CALL FMREGN(POS,IBREG)
      IF(IBREG.NE.IBOLD.AND.IBREG.GT.0) THEN
        CALL FFIELD(POS,IBREG,B)
        IBOLD=IBREG
      ENDIF
C
C? Go to local coordinates
C
      TBON=FLOAT(IBON)
      UBBX= (B(1)*COSTA+B(2)*SINTA)*TBON
      UBBY= (B(1)*SINTA-B(2)*COSTA)*TBON
      UBBZ= (B(3))*TBON
C
      UBBT=SQRT(UBBX*UBBX+UBBY*UBBY+UBBZ*UBBZ)
C
C? Long. and. transv. p with and without bending and transv. B eval.
C
      IF(UBBT.GE.TBCUT)
     +          TPP=(TTIN(4)*UBBX+TTIN(5)*UBBY+TTIN(6)*UBBZ)/UBBT
      TPT2=TPEV*TPEV-TPP*TPP
      IF(TPT2.LE.0.) THEN
        TPT=0.0001
      ELSE
        TPT=SQRT(TPT2)
      ENDIF
C
      IF(UBBT.LT.TBCUT.OR.TPT.LT.TPTCU) THEN
C Linear tracking (no field or small pt)
        ITKR=1
        IF(IREG.LE.2) THEN
          TTOUT(1)=STEPK
          TTOUT(2)=TTIN(5)/TTIN(4)*STEPK
          TTOUT(3)=TTIN(6)/TTIN(4)*STEPK
        ELSE
          TTOUT(1)=TTIN(4)/ABS(TTIN(6))*STEPK
          TTOUT(2)=TTIN(5)/ABS(TTIN(6))*STEPK
          TTOUT(3)=STEPK*FLOAT(ISIDE)
        ENDIF
        DO 703 K=1,3
  703   TTOUT(K)=TTOUT(K)+TTIN(K)
        GOTO 720
      ELSE
C Curved tracking
        ITKR=0
      ENDIF
C
C? Case Bx=0 & By=0 (i.e. Br=0)
C  (By can be zero even if Bx is not zero). Reduced tranform matrix
C
      IF(ABS(UBBX/UBBT).LT.0.1.AND.ABS(UBBY/UBBT).LT.0.1) THEN
        TPT=SQRT(TTIN(4)*TTIN(4)+TTIN(5)*TTIN(5))
        TPP=TTIN(6)
        IBZSN=SIGN(1.,UBBZ)
        TPP=TPP*IBZSN
        CPHPS=TTIN(4)/TPT
        SPHPS=TTIN(5)/TPT
        VA(1,1)=CPHPS
        VA(1,2)=SPHPS
        VA(1,3)=0.
        VA(2,1)=-SPHPS*FLOAT(IBZSN)
        VA(2,2)=CPHPS*FLOAT(IBZSN)
        VA(2,3)=0.
        VA(3,1)=0.
        VA(3,2)=0.
        VA(3,3)=IBZSN
      ELSE
C
C? Case Br not zero
C
        CTH=UBBZ/UBBT
        STH2=ABS(1.-CTH*CTH)
        STH=SQRT(STH2)
        SPH=UBBX/UBBT/STH
        CPH=-UBBY/UBBT/STH
        SPSI=(TTIN(6)-CTH*TPP)/STH/TPT
        IF(ABS(SPSI).GT.1.) SPSI=SPSI/ABS(SPSI)
        CPSI=(TTIN(5)+STH*CPH*TPP-CTH*CPH*SPSI*TPT)/SPH/TPT
        IF(ABS(CPSI).GT.1.) CPSI=CPSI/ABS(CPSI)
C
C? Full transformation matrix
C
        VA(1,1)=CPSI*CPH-CTH*SPH*SPSI
        VA(1,2)=CPSI*SPH+CTH*CPH*SPSI
        VA(1,3)=SPSI*STH
        VA(2,1)=-SPSI*CPH-CTH*SPH*CPSI
        VA(2,2)=-SPSI*SPH+CTH*CPH*CPSI
        VA(2,3)=CPSI*STH
        VA(3,1)=STH*SPH
        VA(3,2)=-STH*CPH
        VA(3,3)=CTH
      ENDIF
C
C? Evaluation kinematic variables in mag. field
C
      UOMEG=UOM*UBBT/TEEV
      URAG=URADI*TPT/UBBT
      VPP=UOMEG*URAG*TPP/TPT
C
C? Tracking in the barrel: intersection between x=k plane and an helix i
C  quadratic approximation: small wt
C
      IF(IREG.LE.2) THEN
        UAAA=TQ*VA(2,1)*URAG*(UOMEG*UOMEG)
        UBBB=2.*(VA(1,1)*URAG*UOMEG+VA(3,1)*VPP)
        ISIK=1
C
C? Tracking in the end cap: intersection with z=k plane
C
      ELSE
        UAAA=TQ*VA(2,3)*URAG*(UOMEG*UOMEG)
        UBBB=2.*(VA(1,3)*URAG*UOMEG+VA(3,3)*VPP)
        ISIK=ISIDE
      ENDIF
      STEPK=STEPK*FLOAT(ISIK)
C
C? Resolution of quadratic equat.(both cases)
C
      CCC=-2.*STEPK
      IF(ABS(UBBB).LT.1E-6) THEN
        TTT=SQRT(ABS(-CCC))
        GOTO 715
      ENDIF
      IF(ABS(UAAA).LT.1E-5) THEN
        TTT=-CCC/UBBB
      ELSE
        SDELT=UBBB*UBBB-4.*UAAA*CCC
        IF(SDELT.LT.0.) THEN
          IFAIL=3
          GOTO 777
        ENDIF
        SQDE=SQRT(SDELT)
        TT1=ABS((-UBBB+SQDE)/(2.*UAAA))
        TT2=ABS((-UBBB-SQDE)/(2.*UAAA))
C
C? Choice of the smallest of t (parametric coordinate)
C
        TTT=TT1
        IF(TT2.LT.TT1) TTT=TT2
      ENDIF
  715 WT=TTT*UOMEG
C
C? Equation of motion in primed coord. system
C
      VRPR(1)=URAG*WT
      VRPR(2)=TQ*URAG*WT*WT/2.
      VRPR(3)=VPP*TTT
C
C? Antitransf. and traslation to actual coord.
C
      DO 709 K=1,3
        TTOUT(1)=TTOUT(1)+VA(K,1)*VRPR(K)
        TTOUT(2)=TTOUT(2)+VA(K,2)*VRPR(K)
  709 TTOUT(3)=TTOUT(3)+VA(K,3)*VRPR(K)
      IF(IREG.LE.2) THEN
        TTOUT(1)=STEPK
      ELSE
        TTOUT(3)=STEPK
      ENDIF
      DO 704 K=1,3
  704 TTOUT(K)=TTOUT(K)+TTIN(K)
C
C? Is the particle still in hcal?
C
  720 ABSTK=ABS(STEPK)
      TROUT=SQRT(TTOUT(1)*TTOUT(1)+TTOUT(2)*TTOUT(2))
      TABO3=ABS(TTOUT(3))
      IF(TROUT.GT.HREMIN.AND.TROUT.LT.TEMAX.AND.
     +                  TABO3.LT.(HZEMAX+0.01)) THEN
        GOTO 730
      ELSE
        IF(IROLD.GE.3) THEN
          IFAIL=1
          GOTO 777
        ENDIF
        IF(TABO3.GT.(HZBMAX+0.01).OR.
     +                  TTOUT(1).GT.(HXBMAX+0.01)) THEN
          IFAIL=1
          GOTO 777
        ENDIF
      ENDIF
C
C? Assignment of region to outgoing particle
C
  730 IF((TTOUT(1)).GE.(HXBMIN-0.01)) THEN
        IF(TABO3.LE.ZBMED) THEN
          IREG=1
          GOTO 741
        ENDIF
        IF(TABO3.LT.HZBMAX) THEN
          IREG=2
        ELSE
          IREG=3
        ENDIF
        GOTO 741
      ENDIF
      IF(TABO3.GE.HZBMAX) THEN
        IREG=3
        GOTO 741
      ENDIF
      IF(TROUT.GT.HREMED) THEN
        IREG=5
        TDEN=0.
      ELSE
        IREG=4
        GOTO 741
      ENDIF
      IF(TROUT.GT.CRMIN.AND.TROUT.LE.CRMAX.AND.
     +          TABO3.GE.(HZEMIN-0.01).AND.TABO3.LT.CZMAX) THEN
        IREG=6
      ENDIF
C
C? Evaluation of total curved track lengths
C
  741 TLEN=SQRT((TTOUT(1)-TTIN(1))*(TTOUT(1)-TTIN(1))+
     +            (TTOUT(2)-TTIN(2))*(TTOUT(2)-TTIN(2))+
     +            (TTOUT(3)-TTIN(3))*(TTOUT(3)-TTIN(3)))
      TCORR=1.
      IF(IBON.EQ.1.AND.ITKR.EQ.0) TCORR=1.+TLEN*TLEN/URAG/URAG/8.
      TLEN=TLEN*TCORR
      OTLE=OTLE+TLEN
C
C? Energy loss and check on residual energy
C
      TDEV=FLOAT(IDEON)*TDEN*ULAST*TLEN*
     +       (ALOG(TBEV*TGEV)-TBEV*TBEV-UDE2)*UDE1*UDE3/TBEV/TBEV
      IF(IREG.EQ.6) THEN
        TDEV=FLOAT(IDEON)*TLEN*
     +       (ALOG(TBEV*TGEV)-TBEV*TBEV-CDE2)*CDE1*CDE3/TBEV/TBEV
      ENDIF
      TEEV=TEEV-TDEV
      IF(TEEV.LE.W2MU) THEN
        IFAIL=4
        GOTO 777
      ENDIF
      TPEV2=TEEV*TEEV-WMU2
      IF(TPEV2.LE.0) TPEV2=0.0001
      TPEV=SQRT(TPEV2)
C
C? Evaluation of outgoing momenta
C
      IF(UBBT.LT.TBCUT.OR.TPT.LT.TPTCU) THEN
        DO 701 J=4,6
  701   TTOUT(J)=TTIN(J)*TPEV/TPOLD
      ELSE
        VVPR(1)=(URAG*UOMEG*(1.-WT*WT/2.))/CLGHT
        VPPR(1)=VVPR(1)*TEEV
        VVPR(2)=TQ*(URAG*TTT*UOMEG*UOMEG)/CLGHT
        VPPR(2)=VVPR(2)*TEEV
        VVPR(3)=VPP/CLGHT
        VPPR(3)=VVPR(3)*TEEV
        DO 710 K=1,3
          TTOUT(4)=TTOUT(4)+VA(K,1)*VPPR(K)
          TTOUT(5)=TTOUT(5)+VA(K,2)*VPPR(K)
  710   TTOUT(6)=TTOUT(6)+VA(K,3)*VPPR(K)
      ENDIF
C
C? Procedure for the correction of the coordinates
C
C? In which module of the barrel is the particle ?
C
      IF(IROLD.LE.2.AND.IREG.LE.2) THEN
        TQP=TTOUT(1)-TA*TTOUT(2)
        TQM=TTOUT(1)+TA*TTOUT(2)
        IF(TQP.GT.0..AND.TQM.GT.0.) GOTO 722
        IF(TQP.GT.0..AND.TQM.LT.0.) THEN
          YM=-TTOUT(1)/TA
          TIMOD=TIMOD+1.
          IF(TIMOD.EQ.13.) TIMOD=1.

          TTOUT(2)=ABS(YM)-(YM-TTOUT(2))*CO30+
     +      (YM-TTOUT(2))*SI30*(1.-ABS(TTOUT(5))/TTOUT(4)*TG30)/
     +      (ABS(TTOUT(5))/TTOUT(4)+TG30)
          TOUT=TTOUT(4)*CO30-TTOUT(5)*SI30
          TTOUT(5)=TTOUT(4)*SI30+TTOUT(5)*CO30
          TTOUT(4)=TOUT
          GOTO 722
        ENDIF
        IF(TQP.LT.0..AND.TQM.GT.0.) THEN
          YM=TTOUT(1)/TA
          TIMOD=TIMOD-1.
          IF(TIMOD.EQ.0.) TIMOD=12.
          TTOUT(2)=-YM+(TTOUT(2)-YM)*CO30-(TTOUT(2)-YM)*SI30*
     +          (1.-TTOUT(5)/TTOUT(4)*TG30)/(TTOUT(5)/TTOUT(4)+TG30)
          TOUT=TTOUT(4)*CO30+TTOUT(5)*SI30
          TTOUT(5)=-TTOUT(4)*SI30+TTOUT(5)*CO30
          TTOUT(4)=TOUT
          GOTO 722
        ENDIF
      ENDIF
C
C? Is still the particle in the same region? Corrections to coord.
C
      IF((IROLD.EQ.IREG).OR.(IROLD.EQ.4.AND.IREG.EQ.3)) GOTO 722
      IF(IROLD.EQ.2.AND.IREG.EQ.3) THEN
        ZDELT=FLOAT(ISIDE)*HZBMAX-TTIN(3)
        XDELT=ABS(STEPK*ZDELT/(TTOUT(3)-TTIN(3)))
        TTOUT(1)=TTIN(1)+XDELT
        TTOUT(2)=TTIN(2)+XDELT*(TTOUT(2)-TTIN(2))/ABSTK
        TTOUT(3)=FLOAT(ISIDE)*HZBMAX
        GOTO 722
      ENDIF
      IF(IROLD.EQ.5.AND.IREG.LE.2) THEN
        TTOUT(3)=FLOAT(ISIDE)*ABSTK*
     +                  (HXBMIN-TTIN(1))/(TTOUT(1)-TTIN(1))+TTIN(3)
        XDELT=HXBMIN-TTIN(1)
        TTOUT(1)=HXBMIN
        TTOUT(2)=TTIN(2)+XDELT*(TTOUT(2)-TTIN(2))/ABSTK
        GOTO 722
      ENDIF
C
C? Release & refill of coordinates (no filling for reg. 5 & 6)
C
  722 CONTINUE
      DO 711 L=1,6
  711 TTIN(L)=TTOUT(L)
      TALPH=(TIMOD-1.)*PIBY6
      COSTA=COS(TALPH)
      SINTA=SIN(TALPH)
      IF(IREG.GE.3) THEN
        NSLAB=1+NINT((ABS(TTOUT(3))-HZEMIN)/HTSAMP)
      ELSE
        NSLAB=1+NINT((TTOUT(1)-HXBMIN)/HTSAMP)
      ENDIF
C
      IF(IREG.LT.5.AND.NVETT.LT.NSTEP+4) THEN
        NVETT=NVETT+1
        DO 712 L=1,6
  712   ATMOD(L,NVETT)=TTOUT(L)
        ATMOD(7,NVETT)=TIMOD
        ATMOD(9,NVETT)=IREG
        ATMOD(10,NVETT)=NSLAB
        VECOS(NVETT)=COSTA
        VESIN(NVETT)=SINTA
      ENDIF
C
C? Continues loop
C
      GOTO 1
C
C? Transformation from module coord. to Y X Z frame and to ALEPH coord.
C
  777 DO 713 L=NVET1,NVETT
        COSTA=VECOS(L)
        SINTA=VESIN(L)
C
C? Coord. and momenta
C
        DO 714 J=1,4,3
          ATFRA(J,L)  =ATMOD(J,L)*COSTA+ATMOD(J+1,L)*SINTA
          ATFRA(J+1,L)=ATMOD(J,L)*SINTA-ATMOD(J+1,L)*COSTA
  714   ATFRA(J+2,L)=ATMOD(J+2,L)
C       ATFRA(7,L)=ATMOD(8,L)
C
C? Barrel or end cap modules (necessary because no changement of
C  module made in the endcaps)
C
        A1=ATFRA(1,L)
        A2=ATFRA(2,L)
        A3=ATFRA(3,L)
        IF(ATMOD(9,L).LT.2.5) THEN
          ATFRA(8,L)=3000.+100.*ATMOD(9,L)+ATMOD(7,L)
        ELSE
          TALEC=ATAN2(A2,A1)
          IF(TALEC.LT.0.) TALEC=TALEC+TWOPI
          IMOEC=TALEC/PIBY6+1.5
          IF(IMOEC.GT.12) IMOEC=IMOEC-12
          ATFRA(8,L)=3000.+100.*ATMOD(9,L)+FLOAT(IMOEC)
        ENDIF
        IF(ATMOD(9,L).GT.4.5) ATFRA(8,L)=0.
  713 ATFRA(9,L)=ATMOD(10,L)

C
      RETURN
      END
C
C
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE ADFILI(FNAME,IER)
C
C   Author    Christoph Grab
C              3-Aug-89
C             Adapted from WD.Schlatters routines
C
C! Add FILI bank  ,  Nr = 0
C  Entries:  ADFILO
C
C ---------------------------------------------------------------------
      INCLUDE 'A_BCS.INC'
      CHARACTER *120 FFNAM
      CHARACTER *49 TEXT
      CHARACTER *(*) FNAME
      CHARACTER *4 NAME
      LOGICAL LEX

      NAME='FILI'
      NI=1
      GOTO 10

      ENTRY ADFILO(FNAME,IER)
C-----------------------------------------------------------------------
      NAME='FILO'
      NI=2
      GOTO 20
C
   10 CONTINUE
      IF(FNAME.EQ.'    ') GOTO 20

C               ADD TYPE

      I1=INDEX(FNAME,'.')
      LE=LENOCC(FNAME)
      IF(I1.EQ.0) THEN
        FFNAM=FNAME
        FNAME=FNAME(1:LE)//'.EDIR'
        CALL AINQUI(0,FNAME,LEX)
        IF(.NOT.LEX)   FNAME=FFNAM(1:LE)//'.NATIVE'
      ENDIF
      CALL AINQUI(0,FNAME,LEX)
      IF(.NOT.LEX) THEN
        WRITE(TEXT,'(A6,A,A10)') ' File ',FNAME,' not found'
        CALL DWRC
        IER=-1
        RETURN
      ENDIF
   20 IER=0
C            DROP LAST BANK IF REQUESTED
      IF(FNAME.EQ.'    ') THEN
        IND=NDROP(NAME,0)
        RETURN
      ENDIF

      IND=ACARD1(' ')
      LBK = (LE+3)/4
      IND=NBANK(NAME,0,LBK)

      LE=LENOCC(FNAME)
      DO 1 IBG=1,LE,4
        I=(IBG+3)/4
    1 IW(IND+I)=INTCHA(FNAME(IBG:IBG+3) )
      RETURN
      END
