      SUBROUTINE CAEPAR
C ----------------------------------------------------------------
C       J.Badier - 16/03/87
C! E.M shower coefficients
C ----- Calcul des coefficients de parametrisation d'une gerbe
C       electromagnetique.
C ----- NATGER = 1 : Electron.
C ----- NATGER = 2 : Photon.
C ----- NATGER = 3 : Pi 0
C   - Called by EHGERB
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
      IF (EMGNRJ .LT. EMINLO) THEN
         EMALFA = 1.
         EMBETA = BETMAX
      ELSE
         ENLOG = ALOG( EMGNRJ )
         I = NATGER
         IF( EMGNRJ .LT. ETRANS .AND. NATGER .EQ. 1 ) I = 4
C ----- Shower parameters.
         ASURB = ASURB1(I) + ASURB2(I) * ENLOG
         UNSRB = UNSRB1(I) + UNSRB2(I) * ENLOG
C ----- Fluctuation.
         IF( EMGNRJ .GT. SEUSIG ) THEN
           SIGA = SQRT( SIGMA1(I) + SIGMA2(I) / EMGNRJ )
           SIGB = SQRT( SIGMB1(I) + SIGMB2(I) / EMGNRJ )
         ELSE
           SIGA = SIGMA3(I)
           SIGB = SIGMB3(I)
         ENDIF
         CALL RANNOR( ALEA1 , ALEA2 )
         SIGA = SIGA * ALEA1
         SIGB = SIGB * ALEA2
C ----- Calculate Alpha and Beta with some protections.
         UNSRA = ( 1. + SIGA ) * UNSRB / ASURB
         IF ( UNSRA .LE. USAMIN ) UNSRA = USAMIN
         IF ( UNSRA .GE. USAMAX ) UNSRA = USAMAX
         EMALFA = 1. / UNSRA
         BSURA = ( 1. + SIGB ) / ASURB
         IF ( BSURA .LE. BSUMIN ) BSURA = BSUMIN
         EMBETA = BSURA / UNSRA
         IF ( EMBETA .LT. BETMIN ) EMBETA = BETMIN
         IF ( EMBETA .GT. BETMAX ) EMBETA = BETMAX
      ENDIF
         EMALM1 = EMALFA - 1.
         IF( EMGNRJ .LE. EMINRA ) THEN
           METHCO = 1
         ELSE
           METHCO = 2
           PM2 = EXPRD0 + EXPRD1 * EMGNRJ
           EMAEXP =  1. / PM2
           EMFACT = PM2 / ( RFACT0 + RFACT1 * EMGNRJ )
           EMUDP0 = 5.
           EMUDP2 = .026
         ENDIF
      SMAXLR = -99.
      EMNAME = '    '
      RETURN
      END
