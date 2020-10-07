      SUBROUTINE EHGERB(ENER,IFIN,XGERB,NRJDP)
C ----------------------------------------------------------------
C       J.Badier - 11/12/85
C       J.Badier - 16/03/87 : Adaptation aux gerbes hadroniques.
C! Computes shower points
C    Input : ENER   Energie totale de la gerbe (Gev)
C
C    Le premier appel est une initialisation selon la condition
C           TIRAGE = 'FLUCTUATED' methode standard
C                    'VERY FAST' methode rapide (Defaut)
C       Output  IFIN :  Nombre de points a tirer dans la gerbe
C
C    Aux appels suivants  (IFIN NE 0)
C       Output  XGERB:  Coordonnees du i eme point i=1,IFIN
C               NRJDP   Signal exprime en kev pour ce point.
C   - Called by EHSHOW
C   - Calls POISSN,EHLOTI,EHTRTI,EHDEPS
C ------------------------------------------------------------------
      SAVE
      DIMENSION XGERB(3)
C
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
      COMMON / ECDBUG / BUGSHR
      CHARACTER*16 BUGSHR
C
C
C     EC Analog signals conditions
      COMMON/EHCOND/ TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
      CHARACTER * 16 TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
C
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
      EXTERNAL RNDM
C
      IF ( IFIN .EQ.  0)           THEN
C ----- Initialisation.
         ICOND = 0
C ----- Gerbe electromagnetique.
         EMGNRJ = ENER
         HADNRJ = 0.
         NATGER = 1
C ----- Nombre IEM de points electromagnetiques a tirer.
C ----- PARGV# : Nombre de tirages par gev.
            PISNU = EMGNRJ * PARGV1
         CALL POISSN(PISNU,IEM,IER)
         IF( IEM .GT.  0) THEN
C ----- Initialisation du tirage electromagnetique.
            CALL CAEPAR
C ----- Composante longitudinale.
            CALL EHLOTI( ICOND , XGERB(1) )
         ENDIF
C ----- Nombre de points a tirer.
         IFIN = IEM
      ELSE
C
C ----- Tirage des points.
         IF(ICOND .EQ. 0) DREST = ELIMST( 2 , 'SUP' )
         ICOND = 1
C
C ----- Tirage electromagnetique.
C ----- Longitudinal.
         CALL EHLOTI( ICOND , XGERB(1) )
         XGERB(1) = XGERB(1) * GRADL
         IF( XGERB(1). GT. DREST ) XGERB(1) =
     +   .648 * XGERB(1) + .352 * DREST
C ----- Radial.
         CALL EHTRTI( XGERB(1) , RAD )
         IF (RAD.GT.0.) THEN
            PHI = TWOPI * RNDM(RAD)
            XGERB(2) = RAD * COS(PHI)
            XGERB(3) = RAD * SIN(PHI)
         ELSE
            XGERB(2)=0.
            XGERB(3)=0.
         ENDIF
C ----- Energie deposee.
         CALL EHDEPS(NRJDP)
C
      ENDIF
C
   10 CONTINUE
      RETURN
  800 FORMAT(5X,'CONDITION DE TIRAGE INCONNUE',5X,A16)
      END
