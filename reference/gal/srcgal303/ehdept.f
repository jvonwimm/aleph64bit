      SUBROUTINE EHDEPT (DELT,NRJ)
C.----------------------------------------------------------------
C       J.Badier - 25/3/86
C! Tr. elem energy deposit
C       Depot d'energie pour un segment de trace
C
C           Input :  DELT Longueur du segment (for hadrons..)
C           Output:  NRJ  Energie deposee en KEV
C  - Called by EHCHAR
C  - calls      IUCOMP                              KERNLIB
C.----------------------------------------------------------------
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
C
C     EC Analog signals conditions
      COMMON/EHCOND/ TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
      CHARACTER * 16 TCUTRK,TSMEAR,TIRAGE,TEDEPO,TPARAM
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      PARAMETER (LGKINE=100)
      COMMON/GCKING/KGCASE,NGKINE,GKIN(5,LGKINE),GTOFD(LGKINE)
     &             ,IGFLGK(LGKINE)
C
      COMMON/GCKIN3/GPOS(3,LGKINE)
      REAL GPOS
C
C   14 milieux.
      EXTERNAL RNDM,IUCOMP
      PARAMETER (NMIL=14)
C
      CHARACTER*4 CHAINT,CHAHOL
      CHARACTER*4 TEVOL(NMIL)
C
      DATA TEVOL / 'EBS2', 'EBS1', 'EBS3',
     +             'ECS2', 'ECS1', 'ECS3', 'ECS0',
     +             'EDS2', 'EDS1', 'EDS3', 'EDS0',
     +             'EB12', 'EC12', 'ED12' /
C
C
C ----- Recherche du milieu
      DO 1 MILI = 1 , NMIL
         IF( TRKVOL .EQ. TEVOL(MILI) ) GO TO 2
    1 CONTINUE
      MILI = 0
    2 CONTINUE
C
C  Pour e+,e-,photons
C
C     Traitement  'VERY FAST' provisoirement abandonne.
C     Utilise RHOVIT et FASTNR
C     avec fraction annulee pour ajuster la fluctuation de l'energie.
      IF(ITRKEL(4) .LE. 3)          THEN
C ----- Fluctuation de l'energie perdue
         ALEA = RNDM(ALEA)
         IF ( ALEA .LT. RHODEP ) THEN
            ENRJ = ALEA * FLUCT1(MILI)
         ELSE
            ENRJ = RNDM(ENRJ) * FLUCT2(MILI)
         ENDIF
         NRJ = NINT ( TRKELE(12) *  ENRJ )
      ELSE
C
C  Traitement des Hadrons charges ( Snow ).
C
C
C switch off soft delta rays produced by hadrons and muons
C  they will be taken into account by the landau fluctuations .
C
          IF(CHAHOL(KGCASE).EQ.'DRAY'.AND.GKIN(4,1).LT.0.1)NGKINE = 0
C
C allow some fraction of the nuclear fragments to deposit energy
C
          IF(TRKELE(13).GT.1.0)THEN
            IDEPO = 0
            RR = RNDM(ENRJ)
            IF(RR.LT.FNFRAG)IDEPO = 1
          ELSE
            IDEPO = 1
          ENDIF

          IF (TRKELE(7) .GT. 0.
     &       .AND. IDEPO .EQ.  1
     &       .AND. TRKELE(14) .NE. 0.)    THEN
             GABE = 0.5 * ( TRKELE(7) + TRKNXT(7) ) / TRKELE(13)
             CALL EBEBLO( GABE , TRKELE(14) , TRKELE(13) , EP1 )
             IF( EP1 .GT. DESATU ) EP1 = DESATU
             CALL ELANDA( DELT , RLAN )
             ENRJ = DELT * EP1 * RLAN
             NRJ = NINT ( ENRJ * EPICUT(MILI) )
         ELSE
             NRJ = 0
         ENDIF
      ENDIF
C
      RETURN
C
      END
