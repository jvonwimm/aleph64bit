      SUBROUTINE EHCUTG (MFLAG)
C --------------------------------------------------------------
C - J.Badier  F.Ranjard - 870817
C! decide wheither Yes/No a geantino is created
C - MFLAG = 0 no creation
C           1 creation
C - called from   ECHIT                           from this .HLB
C --------------------------------------------------
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
C - user stop particle flag
      PARAMETER (NOMOR=3)
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      LOGICAL FTINO, FMUON, FELEC, FHADC
C - geantino particle
      FTINO = ITRKEL(11).EQ.6
C - muon particle
      FMUON = ITRKEL(11).EQ.5
C - e+/e- particle
      FELEC = ITRKEL(11).EQ.2
C - charged hadron
      FHADC = ITRKEL(11).EQ.4
C
C ---------------------------------------------------------------
C
      MFLAG = 0
      IF( TRKELE(8) .GT. EMINLO .AND. ITRKEL(11) .EQ. 2 .AND.
     +    ITRKEL(7) .NE. 0 ) THEN
        RAD = SQRT ( TRKELE(1)**2+TRKELE(2)**2)
        TGTE = ABS ( RAD / TRKELE (3))
        IF( TGTE .GT. .95 .OR. TGTE .LT. .8 ) MFLAG = 1
      ENDIF
C
      END
