      SUBROUTINE EHLOTI( ICOND , CLOTI )
C.----------------------------------------------------------------
C       J.Badier - 27/ 2/86
C                  Modifie le 13/03/87.
C! Shower longitud. profile
C ----- Tirage longitudinal d'une gerbe .
C   Input:
C       ICOND = 0 phase initialisation
C               1 Tirage d'un point
C   Output:
C       CLOTI  tirage logitudinal en longueurs de radiation.
C
C       METH a ete defini dans la phase d'initialisation
C   Calls EHPALO , RNDM , GAUSS ,GAMMA
C      Apelle EHPALO,RANNOR,RNDM,GAUSS,GAMMA
C       Appele par EHGERB
C ----------------------------------------------------------------
      SAVE
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
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
C
C
C     EC Analog signals conditions
      COMMON/EHCOND/ TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
      CHARACTER * 16 TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
C
C
      EXTERNAL EHPALO , RNDM ,GAUSS ,GAMMA
C
C ----- Initialisation du tirage longitudinal
C
      IF(ICOND .EQ. 0)                  THEN
C
      IPASS = 0
      IF( EMBETA .LT. BETMAX )          THEN
         IF(EMALFA.GT.1.) THEN
C ----- Parametrisation standard
            METH=3
C ----- Preparation des 3 zones de tirage
C ----- REDS = parametrisation reduite integree
            REDS1 = GAUSS( EHPALO , .0 , ZONSH1 , .01 )
            REDS2 = GAUSS( EHPALO , ZONSH1 , ZONSH2 , .01) + REDS1
            REDS3=EXP(EMALM1)*GAMMA(EMALFA)/EMALM1**EMALFA
            PINT1 = REDS1 / REDS3
            PINT2 = REDS2 / REDS3
C
C ----- Preparation des tirages zone 1
            PNU1=REDS1/(EHPALO(ZONSH1)*ZONSH1)
            PS1=ZONSH1*EMALM1/(EMBETA*PINT1**PNU1)
C ----- Preparation des tirages zone 3
            PS3=EMALM1/EMBETA
            PNU3=PS3*(REDS3-REDS2)/EHPALO(ZONSH2)
            PS3=ZONSH2*PS3
C ----- Pour electron primaire.
            ELPR = DEPMIN / EMBETA
            IELPR = 0
            IF ( TIRAGE .EQ. 'VERY FAST' )  IELPR=NINT(ELPR*PARGV1)
            IF ( TIRAGE .EQ. 'FLUCTUATED' ) IELPR=NINT(ELPR*PARGV2)
C
         ELSE
C ----- Tirage exponentiel.
            METH = 2
         ENDIF
      ELSE
C ----- Depot ponctuel.
         METH = 1
      ENDIF
C
C ----- Tirages
C
      ELSE
        IPASS = IPASS + 1
C ----- METH=1 : Depot ponctuel
        IF(METH.EQ.1)                    THEN
          CLOTI = 1.
        ELSE
C ----- METH=2 : Tirage selon une exponentielle
        IF( METH .EQ. 2 .OR. IPASS .LE. IELPR ) THEN
          ALEA=RNDM(METH)
          IF(ALEA.LE..00001) ALEA=.000005
          CLOTI=-ALOG(ALEA)/EMBETA
        ENDIF
C ----- METH=3 : Tirage selon la parametrisation standard
        IF(METH.EQ.3)                    THEN
   10     CONTINUE
          ALEA=RNDM(METH)
C ----- Trois zones .
          IF(ALEA.LT.PINT1)              THEN
C ----- ZONE 1
            IF(ALEA.LE..00001) ALEA=.000005
            CLOTI=PS1*ALEA**PNU1
          ELSE
            IF(ALEA.LT.PINT2)            THEN
C ----- ZONE 2
   20          CLOTI = ZONSH1 + (ZONSH2-ZONSH1) * RNDM(ALEA)
               YVAL = EHPALO(CLOTI)
               YTIR = RNDM(YVAL)
               IF(YTIR.GT.YVAL) GO TO 20
               CLOTI = CLOTI*EMALM1/EMBETA
            ELSE
C ----- ZONE 3
               ALEA = RNDM(ALEA)
               IF(ALEA.LE..00001) ALEA = .000005
               CLOTI = PS3 - PNU3 * ALOG(ALEA)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      ENDIF
C
      RETURN
      END
