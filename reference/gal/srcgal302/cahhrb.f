      SUBROUTINE CAHHRB(ENER,IFIN,XGERB,NRDJ)
C     Deposition of hadronic energy in HCAL
C     M.N Minard  10/05/88
C     ENER  : hadronic energy to be deposited
C     IFIN  : 0  no more points to be generated
C     IFIN  : 1  points have to be generated to deposit
C                energy
C     XGERB : coordinate of generated points
C     NRDJ  : 0 generated point does not deposit energy
C           : #0 it does
C     calls to   CASHAD ,POISSN , CAHTRV
C     called from HCSHOW
C======================================================
      SAVE
      DIMENSION XGERB(*)
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
C
C ----- Version du 16/03/87
      COMMON / EHPASH /
     1 RHODEP,PARGV1,ENMAX1,ENMAX2,PARGV2,NRAPID,RHOVIT(14),
     2 FASTNR(14),FLUCT1(14),FLUCT2(14),EMINPI(14),EPICUT(14),
     3 EMINLO,ETRANS,ASURB1(5),ASURB2(5),UNSRB1(5),UNSRB2(5),
     4 SIGMA1(5),SIGMA2(5),SIGMB1(5),SIGMB2(5),SIGMA3(5),SIGMB3(5),
     5 SEUSIG,USAMIN,USAMAX,BSUMIN,BETMIN,BETMAX,
     6 ZONSH1,ZONSH2,DEPMIN,
     7 EMINRA,EXPRD0,EXPRD1,RFACT0,RFACT1,KPAMAX,X1MAXI,EPSRAD,
     8 EMFRAC,ALPHA0,ALPHA1,BETAH0,BETAH1,RAYHA0,RAYHA1,PUISH0,
     9 PARGVH,NRJHAD,IPLMIN(14),IPLMAX(14),DESATU,FNFRAG,
     + ECHMX,ECHDC,EKEVH,ECHDN,ERADMX,
     + ERMAX,ETANG,ERHMX,EZMAX,EDSELM,EDSHAD,ECUTTE,
     + ST3BA0,ST3EC0,ST3BA1,ST3EC1,ST3BA2,ST3EC2
      COMMON / EHPADA /
     1   CNRJDA,C1PRDA,C2PRDA,C3PRDA,PIMADA,ANRJDA,A1PRDA,A2PRDA,
     2   A3PRDA,A4PRDA,A5PRDA,AMRJDA
      COMMON/GCMATE/NGMAT,NGAMAT(5),GA,GZ,GDENS,GRADL,GABSL
C
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
C
C     EC Analog signals conditions
      COMMON/EHCOND/ TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
      CHARACTER * 16 TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
C
      COMMON /CAHTRA/ HCTRKM (27,2 ),HCAXL0,HTIN01,HTIX01,HSHGRA,HSHGAB
      COMMON/HCLOC/HCPDIP,HCPPHI,HCPACX,HCPACY,HCPACZ ,HCPAX0,HCPAY0,
     +             HCPAZ0,FHADRC,FHCPRJ ,IHCPOR,IHCMOD,IHCIPL
      LOGICAL FHADRC,FHCPRJ
      DATA RHMX / .4 / , TANG / .21132 / ,CHNRJ/ 4.27 /
      IF( IFIN .EQ. 0 ) THEN
        IF( ENER .LT. .001 ) GO TO 2
        IFIN = 1
C ----- Initialisation
C ----- Angle avec le module.
        CSTE = ABS( HCTRKM(6,1))
        CSTE = MAX ( .01 , CSTE )
        TGTE = SQRT( 1. / CSTE ** 2 - 1. )
        IF( ABS( HCTRKM(3,1) ) .LT. 250. ) TGTE = 1. / TGTE
C ----- Limits for generation.
        DELT = RHMX * TGTE / HSHGAB
        SMIN = HTIN01 - DELT * HCTRKM(9,1)
        SMAX = TINOL0 + DELT * HCTRKM(9,2)
C
C
C ----- Integration step.
        DS = .2
        DDS = .5 * DS
        TDS = TANG * DS
C ----- Number of steps.
        ISTP = ( SMAX - SMIN ) / DS
        ISTP = ISTP + 1
C ----- Number of points per abs. len.
        CNRJ = CHNRJ * DDS
C ----- First step initialisation.
        NRDJ = 1
        S1 = SMIN
        S2 = SMIN + DS
        NPT = 0
        INIT = 0
      ELSE
C ----- Points generation.
        NRDJ = 0
        IF( NPT .EQ. 0 ) THEN
    1     CONTINUE
          IF( ISTP .EQ. 0 ) GO TO 2
          ISTP = ISTP - 1
          S1 = S2
          S2 = S1 + DS
C ----- Number of points for this step.
          SP1 = S1 + TDS
          F1 = CASHAD( SP1 , INIT )
          INIT = 1
          SP2 = S2 - TDS
          F2 = CASHAD( SP2 , INIT )
          EF = CNRJ * ( F1 + F2 )
          CALL POISSN( EF , NPT , IER )
          IF( NPT .EQ. 0 ) GO TO 1
          RHO = F1 / ( F1 + F2 )
          IF (RHO.LT.0.0001)RHO=0.0001
          RH1 = DDS / RHO
          IF (RHO.GT.0.999) RHO=0.999
          RH2 = DDS / ( 1. - RHO )
        ENDIF
        ALEA = RNDM(ALEA)
        IF( ALEA .LT. RHO ) THEN
          ST = S1 + RH1 * ALEA
        ELSE
          ST = S2 - RH2 * ( 1. - ALEA )
        ENDIF
        NPT = NPT - 1
        XGERB(1) = ( ST - HTIN01 ) * HSHGAB
        XGERB1 = HCPAX0+XGERB(1)*HCPACX
        XGERB2 = HCPAY0+XGERB(1)*HCPACY
        XGERB3 = HCPAZ0+XGERB(1)*HCPACZ
        DGRB = XGERB(1) + HCTRKM(9,1)
C ----- Radial.
        CALL CAHTRV( DGRB , RAD )
        IF ( RAD .GT. 0. ) THEN
          PHI = TWOPI * RNDM(RAD)
          XGERB(2) = RAD * COS(PHI)
          XGERB(3) = RAD * SIN(PHI)
C
C-      ROTATE SYS
C
        ELSE
          XGERB(2)=0.
          XGERB(3)=0.
        ENDIF
        XGERB(1) = XGERB1+ COS(HCPPHI)*SIN(HCPDIP)*XGERB(2)
     +           - SIN(HCPPHI)*XGERB(3)
        XREL = XGERB(2)
        XGERB(2) = XGERB2 + SIN(HCPPHI)*SIN(HCPDIP)*XGERB(2)
     +             +COS(HCPPHI)*XGERB(3)
        XGERB(3) = XGERB3 - COS(HCPDIP)*XREL
C ----- Energie deposee.
C
      NRDJ = 1
      ENDIF
C
   98 CONTINUE
      RETURN
C ----- Plus de points!
    2 CONTINUE
      IFIN = 0
      GO TO 98
      END
