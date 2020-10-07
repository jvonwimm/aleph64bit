      SUBROUTINE CAHPAR (ENER,IPART)
C ----------------------------------------------------------------
C       J.Badier - 16/03/87
C   Modified by G.Ganis        16/Jun/89
C! computes had. shower param.
C ----- Calcul des parametres de la gerbe hadronique.
C Input: ENER  Energie du track elem.
C        IPART  particle #
C ----- Voir HADCAL.
C ------------------------------------------------------------------
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
      TOTNRJ = ENER
      ENLOG = ALOG( ENER )
      IF(IPART.EQ.LTYPEL.OR.IPART.EQ.LTYPGA) THEN
C ----- Electron geantino/ photon geantino
         EMGNRJ = ENER
         NATGER = 1
         HADNRJ = 0.
         IF (IPART .EQ. LTYPGA) NATGER = 2
      ELSE
C ----- Hadronic geantino.
C ----- Fraction e.m.
C ----- Je tire la queue.
         IF(TOTNRJ .GT. CNRJDA ) THEN
            CDAN = C1PRDA
         ELSE
            CDAN = C2PRDA + C3PRDA * TOTNRJ
         ENDIF
         ALEA = 2. * RNDM(ALEA) / CDAN
         XMIN = PIMADA / TOTNRJ
         SUIL = ( 1. - XMIN ) ** 2
         IF( ALEA .LT. SUIL ) THEN
            FRAC = 1. - SQRT( SUIL - ALEA )
         ELSE
C ----- Je tire la langue.
            IF( TOTNRJ .GT. ANRJDA ) THEN
               ADAN = A1PRDA + A2PRDA * TOTNRJ
            ELSE
               IF( TOTNRJ .GT. AMRJDA ) THEN
                  ADAN = A3PRDA
               ELSE
                  ADAN = A4PRDA + A5PRDA * TOTNRJ
               ENDIF
            ENDIF
            ADAN = ADAN - CDAN
            BADA = ( 2. - CDAN * SUIL ) / ADAN
            BADA = XMIN + SQRT(BADA)
            ALEA = RNDM(ALEA)
            ALEA = SQRT(ALEA)
            FRAC = BADA - ALEA * ( BADA - XMIN )
         ENDIF
         EMGNRJ = FRAC * TOTNRJ
C ----- Fraction hadronique.
         HADNRJ = ENER - EMGNRJ
C ----- Parametres longitudinaux.
         HALPHA = ALPHA0 + ALPHA1 * ENLOG
         HABETA = BETAH0 + BETAH1 * ENLOG
C ----- Parametres transversaux.
         HADRAY = RAYHA0 + RAYHA1 * ENER
         HAPUIS = 1. / ( PUISH0 - 2. )
         HADRAY = HAPUIS / HADRAY
C ----- Nature de la fraction e.m.
         NATGER = 5
      ENDIF
      SMAXLR = -99.
      SMAXLA = -99.
      EMNAME = '    '
      RETURN
      END
