      SUBROUTINE EHPASR
C.----------------------------------------------------------------
C       J.Badier - 27/ 2/86
C       J.Badier - Modifie le 16/03/87.
C! Init param. for signals
C       Initialisation des parametres pour la creation de signal.
C  - Called by ECDFPA
C  - Calls  none
C.----------------------------------------------------------------
      SAVE
      DIMENSION PARGEV(14),DELTAL(14),SIGMAL(14)
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      COMMON / ECDBUG / BUGSHR
      CHARACTER*16 BUGSHR
C
C
C ----- Parametres pour depot d'energie
C
C   Stack 3 normalisation parameters.
C   ST3BA0,ST3EC0 concerns muons and hadrons.
C   ST3BA1,ST3EC1 concerns all deposited energy
C   ST3BA2,ST3EC2 concerns the ROC correction factor.
      ST3BA0 = 1.935
      ST3EC0 = 1.925
      ST3BA1 = 1.932
      ST3EC1 = 1.901
      ST3BA2 = 1.932
      ST3EC2 = 1.901
C ----- Stacks limits.
      IPLMIN(1) = 11
      IPLMIN(2) =  1
      IPLMIN(3) = 34
      IPLMIN(4) = 11
      IPLMIN(5) =  2
      IPLMIN(6) = 34
      IPLMIN(7) =  1
      IPLMIN(8) = 11
      IPLMIN(9) =  2
      IPLMIN(10) = 34
      IPLMIN(11) =  1
      IPLMIN(12) =  1
      IPLMIN(13) =  1
      IPLMIN(14) =  1
C
      IPLMAX(1) = 33
      IPLMAX(2) = 10
      IPLMAX(3) = 45
      IPLMAX(4) = 33
      IPLMAX(5) = 10
      IPLMAX(6) = 45
      IPLMAX(7) =  1
      IPLMAX(8) = 33
      IPLMAX(9) = 10
      IPLMAX(10) = 45
      IPLMAX(11) =  1
      IPLMAX(12) = 33
      IPLMAX(13) = 33
      IPLMAX(14) = 33
C
C saturation at DESATU * mip
C fraction of nuclear fragments which deposit energy = FNFRAG
C
      DESATU = 25.0
      FNFRAG = 0.2
C
C
C ----- DNRJ correspond a d(nrj)/sqrt(nrj)
      DNRJ = .2
C ----- RHO caracterise la queue de Landau(Tirage 'FLUCTUATED')
      RHO = .15
      RHODEP = 1. - RHO
C
C ----- Cas des gerbes.
C       ***************
C ----- Parametres du tirage 'FLUCTUATED' ----------------------
C
C -----  Nombre de tirages par gev.(PARGV1)
      PARGV1 = 150.
      EPS2 = DNRJ ** 2
C ----- Parametres de la distribution.
      EMOY = 1000000. / PARGV1
      D2 = ( 3. * PARGV1 * EPS2 - 4. ) / ( RHO * RHODEP )
      IF(D2 . GT . 0.)                   THEN
         D = SQRT( D2 )
         S = 4. + D * (RHODEP - RHO)
         ENMAX1 = . 5* (S - D) * EMOY / RHODEP
         ENMAX2 = . 5* (S + D) * EMOY
      ELSE
         WRITE (LOUTIO,800)
      ENDIF
C
C ----- Parametres du tirage 'VERY FAST' ---------------------
C
C ----- Nombre de tirages par Gev.(PARGV2)
      PARGV2 = 1. / DNRJ**2
C ----- Valeur de l'energie de tirage .
      RAPID =  1000000. / PARGV2
      NRAPID = NINT( RAPID )
C
C ----- Cas des elements de trace.
C       **************************
C ----- Coupure sur electrons et photons a 5 Mev,champ : 15 kgauss
C ----- Indice = 1,4 pour EB12,EBS3,EC12,ECS3.
C ----- Reevalue le 13 02 87 avec GAL160 et coupures a .005 Gev
      PARGEV(1) = 214.3
      DELTAL(1) = 1./ PARGEV(1)
      SIGMAL(1) = .00366
      PARGEV(2) = 214.3
      DELTAL(2) = 1./ PARGEV(2)
      SIGMAL(2) = .00366
      PARGEV(3) =  208.6
      DELTAL(3) = 1./PARGEV(3)
      SIGMAL(3) = .00346
      PARGEV(4) =  212.8
      DELTAL(4) = 1./PARGEV(4)
      SIGMAL(4) = .00358
      PARGEV(5) =  212.8
      DELTAL(5) = 1./PARGEV(5)
      SIGMAL(5) = .00358
      PARGEV(6) =  207.7
      DELTAL(6) = 1/PARGEV(6)
      SIGMAL(6) = .00348
      PARGEV(7) =  212.8
      DELTAL(7) = 1./PARGEV(7)
      SIGMAL(7) = .00358
      DO 11 I = 4 , 7
      PARGEV( I + 4 ) = PARGEV( I )
      DELTAL( I + 4 ) = DELTAL( I )
      SIGMAL( I + 4 ) = SIGMAL( I )
   11 CONTINUE
      PARGEV(12) = 214.3
      DELTAL(12) = 1./ PARGEV(2)
      SIGMAL(12) = .00366
      PARGEV(13) =  212.8
      DELTAL(13) = 1./PARGEV(5)
      SIGMAL(13) = .00358
      PARGEV(14) =  212.8
      DELTAL(14) = 1./PARGEV(5)
      SIGMAL(14) = .00358
   10 CONTINUE
C
      DO 20 MIL =  1, 14
         EMOY = 1000000. / ( PARGEV(MIL) * DELTAL(MIL) )
C ----- Pour tirage 'VERY FAST'
         UNPS2 = PARGEV(MIL) * DNRJ ** 2
         UNPS2 = UNPS2 / (1. + (SIGMAL(MIL)/DELTAL(MIL))**2)
         RHOVIT (MIL) = 1. / UNPS2
         FASTNR(MIL) = UNPS2 * EMOY
C ----- Pour tirage 'FLUCTUATED'
         D2 = (3. * UNPS2 - 1.) / ( RHO * RHODEP )
         IF(D2 . GT . 0.) THEN
            D = SQRT( D2 )
            S = 4. + D * (RHODEP - RHO)
            FLUCT1(MIL) = . 5* (S - D) * EMOY / RHODEP
            FLUCT2(MIL) = . 5* (S + D) * EMOY
C
C
         ELSE
            WRITE(LOUTIO,800)
         ENDIF
   20 CONTINUE
C
C ----- Parametres pour hadrons.
      EMINPI(1) = 8199.
      EMINPI(2) = 8199.
      EMINPI(3) = 6531.*ST3BA0
      EMINPI(4) = 7448.
      EMINPI(5) = 7448.
      EMINPI(6) = 6045.*ST3EC0
      EMINPI(7) = 7448.
      EPICUT(1) = 13559.
      EPICUT(2) = 13559.
      EPICUT(3) = 13559.*ST3BA0
      EPICUT(4) = 13559.
      EPICUT(5) = 13559.
      EPICUT(6) = 13559.*ST3EC0
      EPICUT(7) = 13559.
      DO 12 I = 4 , 7
      EMINPI( I + 4 ) = EMINPI( I )
      EPICUT( I + 4 ) = EPICUT( I )
   12 CONTINUE
      EMINPI(12) = 8199.
      EPICUT(12) = 13559.
      EMINPI(13) = 7448.
      EPICUT(13) = 13559.
      EMINPI(14) = 7448.
      EPICUT(14) = 13559.
C
C ----- Parametres utilises dans EHLOTI
C
C ----- Energy thresholds.
C ----- Seuil en energie du tirage longitudinal.
      EMINLO = .02
C ----- Energies de transition.
      ETRANS = .1
      SEUSIG = .5
C ----- Electron or positron  shower.Energy > ETRANS
C        Maria Bardadin estimation.
      ASURB1(1) = 5.262
      ASURB2(1) = 1.042
      UNSRB1(1) = 1.857
      UNSRB2(1) =  .044
      SIGMA1(1) = .041
      SIGMA2(1) = .073
      SIGMB1(1) = .013
      SIGMB2(1) = .023
C ----- Electron or positron  shower.Energy < ETRANS
      ASURB1(4) = 6.361
      ASURB2(4) = 1.534
      UNSRB1(4) = 4.309
      UNSRB2(4) = 1.068
      SIGMA1(4) = .041
      SIGMA2(4) = .073
      SIGMB1(4) = .013
      SIGMB2(4) = .023
C ----- Photon shower.
      ASURB1(2) = 4.5
      ASURB2(2) = 1.22
      UNSRB1(2) = 1.96
      UNSRB2(2) =  .0
      SIGMA1(2) = .015
      SIGMA2(2) = .08
      SIGMB1(2) = .01
      SIGMB2(2) = .012
C ----- Shower from 2 photons from a pi zero.
      ASURB1(3) = 4.32
      ASURB2(3) =  .98
      UNSRB1(3) = 2.04
      UNSRB2(3) =  .0
      SIGMA1(3) = .025
      SIGMA2(3) = .085
      SIGMB1(3) = .01
      SIGMB2(3) = .025
C ----- Electromagnetic fraction of hadronic shower.
      ASURB1(5) = 5.
      ASURB2(5) = 0.
      UNSRB1(5) = 1.85
      UNSRB2(5) =  .0
      SIGMA1(5) = .237
      SIGMA2(5) = .0
      SIGMB1(5) = .063
      SIGMB2(5) = .0
C ----- Energies < SEUSIG
      DO 30 I = 1,5
         SIGMA3(I) = SIGMA1(I) + SIGMA2(I) / SEUSIG
         SIGMB3(I) = SIGMB1(I) + SIGMB2(I) / SEUSIG
   30 CONTINUE
C ----- Limites acceptables pour les parametres.
      USAMIN = .1
      USAMAX = 2.5
      BSUMIN = .05
C ----- Valeur minima de beetaa
      BETMIN = .1
C ----- Valeur maxima de beetaa
      BETMAX = 10.
C ----- Definition des zones de tirage longitudinal.
      ZONSH1 = .5
      ZONSH2 = 2.
C
C ----- Energie deposee par longueur de radiation.
      DEPMIN = .016
C
C ----- Parametres utilises dans EHTRTI
C
C ----- Valeur minimum de l'energie lors du tirage radial.
      EMINRA = .15
C ----- Parametres de la distribution radiale deduits des donnees
C       du run test 84.
      EXPRD0 = 1.8
      EXPRD1 = .05
      RFACT0 = 19.2
      RFACT1 = .0
C ----- Controle des iterations de EHTRTI
      KPAMAX = 20
C
C ----- Rapport des longueurs de radiation stack12/stack3.
      RARAD = .647
C
C ----- Parametres pour une gerbe hadronique.
C ----- Fraction e.m.
      PIMADA = .14
      C1PRDA = 1.34
      C2PRDA = .3
      C3PRDA = .047
      CNRJDA = ( C1PRDA - C2PRDA ) / C3PRDA
      A1PRDA = .8
      A2PRDA = .47
      A3PRDA = 8.
      A4PRDA = 33.
      A5PRDA = -12.5
      AMRJDA = ( A3PRDA - A4PRDA ) / A5PRDA
      ANRJDA = ( A3PRDA - A1PRDA ) / A2PRDA
C ----- Coefficients HADRONIQUES PROVISOIRES.......
      ALPHA0=1.123
      ALPHA1=0.
      BETAH0=.921
      BETAH1=0.
      RAYHA0=2.0
      RAYHA1=0.
      PUISH0=2.5
      PARGVH=47.6
      ENHAD=1000000./PARGVH
      NRJHAD = NINT( ENHAD )
CC
C-    SET PARAMETERS USED IN EHDEPH
C
      ECHMX = 750.
      EKEVH = 41.
      ECHDC = 615.
      ECHDN = 600.
C
C--   PARAMETERS USED IN ELM SHOWER PARAMETRISATION
C
      ERMAX = 0.4
      ERHMX = 0.4
      ETANG = 0.21132
      EZMAX = 250.
      ERMAX2 = 40000.
      EDSELM = 0.2
      EDSHAD = 0.2
      ERADMX = 25.
C
C - total energy before the step below which a particle is not FAST trac
C   (used in EHCUTF)
      ECUTTE = 0.250
C
      RETURN
  800 FORMAT (5X,'PARAMETRES DEPOT ENERGIE INCOHERENTS')
      END
