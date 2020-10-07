From BLOCH@alws.cern.ch Fri Feb 13 15:58:49 2004
Date: Fri, 13 Feb 2004 15:56:36 +0100
From: BLOCH@alws.cern.ch
To: BLOCH@alws.cern.ch

C*HE 01/17/91 12:27:59 C
C*DK ASKUSE
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C --------------------------------------------------------------------
C!Generation
C --------------------------------------------------------------------
C--------------------------------------------------------------------
C     input     : none
C
C     output    : 6 arguments
C          IDPR   : process identification
C          ISTA   : status flag ( 0 means ok)
C          NTRK   : number of tracks generated and kept
C          NVRT   : number of vertices generated
C          ECMS   : center of mass energy for the event
C          WEIT   : event weight none always equal to 1
C--------------------------------------------------------------------
C*CA BCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=50000)
      COMMON / BCS / IW(LBCS )
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C*CA MISCEL
      REAL*4 ECM,WEI,VRTEX,SDVRT
      COMMON / MISCEL / IST,IDP,ECM,WEI,NEVENT(6)
      COMMON / VERTEX / VRTEX(4),SDVRT(3)
C*CC MISCEL
C*CA GPARAM
      REAL ZM,GZ,S2THEW
      COMMON / PARWEAK / ZM,GZ,S2THEW
      REAL EB,XMCHI
      INTEGER IFL,IFLO,ITYP,ISGN
      COMMON / GPARAM / EB,XMCHI(2),IFL(2),IFLO(2),ITYP,ISGN
C*CC GPARAM
C
      IDPR = 0
      ISTA = 0
      NTRK = 0
      NVRT = 0
      WEIT = 1.
C
C  Generate primary vertex
C
      CALL RANNOR (RN1,RN2)
      CALL RANNOR (RN3,DUM)
      VRTEX(1) = RN1*SDVRT(1)
      VRTEX(2) = RN2*SDVRT(2)
      VRTEX(3) = RN3*SDVRT(3)
      VRTEX(4) = 0.
C
C  Event generation
C
      CALL CHICHI(ISTA)
C
      IF(ISTA.NE.0) THEN
       NEVENT(4) = NEVENT(4) + 1
       NEVENT(3) = NEVENT(3) + 1
       GO TO 10
      ENDIF
C
C  Event header information
      IDPR= ITYP*1000000 + IFLO(1)*1000 + IFLO(2)
      ECMS= 2.*EB
C
C  Book all banks
C
      CALL KXLUAL(VRTEX,ISTA,NVRT,NTRK)
      IF(ISTA.NE.0) THEN
       NEVENT(5) = NEVENT(5) + 1
       NEVENT(3) = NEVENT(3) + 1
       GO TO 10
      ENDIF
C
C  Event counters
C
      NEVENT(2) = NEVENT(2) + 1
C
   10 CONTINUE
      RETURN
      END
C*DK ASKUSI
      SUBROUTINE ASKUSI(IGCOD)
C--------------------------------------------------------------------
C!Initialization
C--------------------------------------------------------------------
C*CA BCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=50000)
      COMMON / BCS / IW(LBCS )
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C*CA MISCEL
      REAL*4 ECM,WEI,VRTEX,SDVRT
      COMMON / MISCEL / IST,IDP,ECM,WEI,NEVENT(6)
      COMMON / VERTEX / VRTEX(4),SDVRT(3)
C*CC MISCEL
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (L4CHAG=50, L4CHAF=100)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      REAL*4 PARLU1,PMASL2,PWIDL2,CFRLU2,DPARL3,CBRLU3,PARELE,PARTLU
      CHARACTER*4 CHAGL4,CHAFL4
      COMMON / LUDAT1 / MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON / LUDAT2 / KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID),
     &                  KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON / LUDAT3 / DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR),
     &                  KDPLU3(L3KDP)
      COMMON / LUDAT4 / CHAGL4(L4CHAG),CHAFL4(L4CHAF)
      COMMON / LUDATE / MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON / LUJETS / NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C*CC LUNDCOM
      PARAMETER( IGCO = 7007 )
      PARAMETER( NTABL= 14 )
      PARAMETER( LPDEC= 48 )
      REAL TABL(NTABL)
      INTEGER NODEC(LPDEC)
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL,ALRLEP
C
C  Return generator code
C
      IGCOD=IGCO
      WRITE (IW(6),1000) IGCOD
 1000 FORMAT(30X,78('*'),/,40X,'Welcome to the Neutralino  MC CHI001'
     $    ,/,70X,'GENERATOR CODE IS :',I10,/,
     $     40X,'Last date of modification is  ',
     $     ' November 13 , 1990',
     $   /,30X,78('*'),/)
C
C  Input parameters for the generator
C  To be provided on the GENE card
C
      IPRT =0
      IGENE = NLINK('GENE',0)
      IF(IGENE.NE.0) THEN
       DO 1 L=1,11
        IF(L .LE. 3 .OR. L .GE. 9) THEN
         TABL(L) = RW(IGENE+L)
        ELSE
         TABL(L) = FLOAT(IW(IGENE+L))
        ENDIF
 1     CONTINUE
       IPRT = IW(IGENE+12)
C
C  Main vertex initialization
C
      SDVRT(1) = 0.035
      SDVRT(2) = 0.0012
      SDVRT(3) = 1.28
      JSVRT = NLINK('SVRT',0)
      IF(JSVRT.NE.0) THEN
       SDVRT(1) = RW(JSVRT+1)
       SDVRT(2) = RW(JSVRT+2)
       SDVRT(3) = RW(JSVRT+3)
      ENDIF
      DO 2 L=1,3
       L1= 11 + L
       TABL(L1)= SDVRT(L)
2     CONTINUE
C
C  Generator initialisation
C
      CALL CHIINIT(TABL)
C
C  Fill the KPAR bank with the generator parameters
C
      NCOL = NTABL
      NROW = 1
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
      CALL PRTABL('KPAR',0)
C
C  Fill the RLEP bank
C
      IEBEAM= NINT(TABL(1)*1000)
      JRLEP= ALRLEP(IEBEAM,'    ',0,0,0)
      CALL PRTABL('RLEP',0)
C
      ENDIF
C
C  Initialization event counters
C
      DO 20 I = 1,6
       NEVENT(I) = 0
20    CONTINUE
C
C  update Lund parameters from data cards
C
      CALL KXLUCO(LUPAR)
C
C  Taus may be produced in subsequent processes...give a chance
C  to use modified Tau decays in LUND
C
      IFT = 0
      CALL LUTAUD(IFT)
      IF (IFT.NE.0) THEN
       WRITE(6   ,'(1X,''LUTAUD error  - STOP -'')')
       CALL EXIT
      ENDIF
C
      CALL KXLUPA(IPART,IKLIN)
      IF (IPART.LE.0.OR.IKLIN.LE.0) THEN
       WRITE(6   ,'(1X,''ASKUSI:error in PART or KLIN bank
     &             - STOP -'',2I3)')IPART,IKLIN
       CALL EXIT
      ENDIF
      IF (IPRT.GT.0) CALL PRPART
C
C
C   Inhibit decays
C
      MXDEC=KNODEC(NODEC,LPDEC)
      MXDEC=MIN(MXDEC,LPDEC)
      IF (MXDEC.GT.0) THEN
         DO 10 I=1,MXDEC
            IF (NODEC(I).GT.0) IDBLU3(NODEC(I))=0
   10    CONTINUE
      ENDIF
      RETURN
      END
C*DK BRADEC
      SUBROUTINE BRADEC(IER)
C-------------------------------------------------------------------
C! Calculate branching ratios for chip decay
C-------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (L4CHAG=50, L4CHAF=100)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      REAL*4 PARLU1,PMASL2,PWIDL2,CFRLU2,DPARL3,CBRLU3,PARELE,PARTLU
      CHARACTER*4 CHAGL4,CHAFL4
      COMMON / LUDAT1 / MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON / LUDAT2 / KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID),
     &                  KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON / LUDAT3 / DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR),
     &                  KDPLU3(L3KDP)
      COMMON / LUDAT4 / CHAGL4(L4CHAG),CHAFL4(L4CHAF)
      COMMON / LUDATE / MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON / LUJETS / NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C*CC LUNDCOM
C*CA MISCEL
      REAL*4 ECM,WEI,VRTEX,SDVRT
      COMMON / MISCEL / IST,IDP,ECM,WEI,NEVENT(6)
      COMMON / VERTEX / VRTEX(4),SDVRT(3)
C*CC MISCEL
C*CA CHIDEC
      COMMON / CHIDEC / T3F(4),QF(4),XMF(11),GAM(11),DGAM(11),BR(11),
     +                  EX2MAX(11),DA2MAX(11),AF(4),BF(4),
     +                  DA1MAX,BWMAX,
     +                  EX(2),PX(2)
C*CC CHIDEC
C*CA GPARAM
      REAL ZM,GZ,S2THEW
      COMMON / PARWEAK / ZM,GZ,S2THEW
      REAL EB,XMCHI
      INTEGER IFL,IFLO,ITYP,ISGN
      COMMON / GPARAM / EB,XMCHI(2),IFL(2),IFLO(2),ITYP,ISGN
C*CC GPARAM
      DIMENSION QMUP(4),QMUM(4),QZSTAR(4),QXO(4),QXI(4)
      DIMENSION QWK(4)
C
      DATA SMALL2/0.0001/
      DATA NLMAX /10000/
C
      IER= 0
      PI=4.*ATAN(1.)
      XMI= XMCHI(2)
      XMO= XMCHI(1)
C
C---Reading the event parameters
C
      IFER = 0
 4    CONTINUE
      IFER = IFER +1
      IF(IFER.GT.11) THEN
        GTOT=0.
        DO 11 IFER=1,11
          GTOT=GTOT+GAM(IFER)
 11     CONTINUE
        IF(GTOT.EQ.0.) GOTO 998
        DO 12 IFER=1,11
          BR(IFER)=GAM(IFER)/GTOT
 12     CONTINUE
        GOTO 999
      ENDIF
      DA2MAX(IFER)=0.
      GAM(IFER)=0.
      DGAM(IFER)=0.
      IF ((XMI-XMO).LE.(2.*XMF(IFER)+SMALL2)) GOTO 4
      KFER  = KFLAV(IFER)
      NTIR = 1
C
 20   CONTINUE
C
      A=RNDM(A)
      QXO(4)=(EX2MAX(IFER)-XMO)*A+XMO
      DISTR=SQRT(QXO(4)**2-XMO**2)
      A=RNDM(A)
      IF (DISTR.LT.(EX2MAX(IFER)*A)) GOTO 20
C
C---- Chi2 energy distribution ( chip c.m.s. )
      ZSTARM=SQRT(XMO**2+XMI**2-2.*XMI*QXO(4))
C
C---- TETA2 between chip and Z*, and PHI2
      A=RNDM(A)
      CTETA2=2.*(A-0.5)
      A=RNDM(A)
      PHI2=2.*PI*A
C
C---- TMU between fermions and Z*, and PHIMU
      A=RNDM(A)
      CTMU=2.*A-1.
      A=RNDM(A)
      PHIMU=2.*PI*A
C
C---- Fermion 4-momentmum ( Z* c.m.s. )
      QMUP(4)= ZSTARM/2.
      PMUP   = SQRT(QMUP(4)**2-XMF(IFER)**2)
      SMU    = SQRT(1.-CTMU**2)
      QMUP(1)= PMUP*SMU*COS(PHIMU)
      QMUP(2)= PMUP*SMU*SIN(PHIMU)
      QMUP(3)= PMUP*CTMU
C
      QMUM(4)= QMUP(4)
      QMUM(1)=-QMUP(1)
      QMUM(2)=-QMUP(2)
      QMUM(3)=-QMUP(3)
C
C---- Z* 4-momentum
      QZSTAR(4)= XMI-QXO(4)
      QZSTAR(1)= 0.
      QZSTAR(2)= 0.
      QZSTAR(3)= SQRT(QZSTAR(4)**2-ZSTARM**2)
C
C---- Lorentz trans. to chip c.m.s. for fermions
      BETAX = 0.
      BETAY = 0.
      BETAZ = -QZSTAR(3) / QZSTAR(4)
      CALL LORENZ(BETAX,BETAY,BETAZ,QMUM)
      CALL LORENZ(BETAX,BETAY,BETAZ,QMUP)
C
C---- Chi2 4-momentum ( chip c.m.s. )
      PXO=SQRT(QXO(4)**2-XMO**2)
      STETA2=SQRT(1.-CTETA2**2)
      QXO(1)=-PXO*STETA2*COS(PHI2)
      QXO(2)=-PXO*STETA2*SIN(PHI2)
      QXO(3)=-PXO*CTETA2
C
C---- Rotation of fermions within chip c.m.s.
      CALL ROT3D(CTETA2,PHI2,QMUP,QMUP)
      CALL ROT3D(CTETA2,PHI2,QMUM,QMUM)
C
C---- Angular distribution for chi2 and fermions ( chip c.m.s. )
      D2=QMUP(4)*PS(QMUM,QXO)+QMUM(4)*PS(QMUP,QXO)
     &         +ISGN*XMO*PS(QMUP,QMUM)
      D2P= QXO(4)+2.*ISGN*XMO
      DANG2=((AF(KFER)**2+BF(KFER)**2)*D2
     &         +2.*(XMF(IFER)**2)*AF(KFER)*BF(KFER)*D2P)
C
C---  Maximum weight
      IF(DANG2.GT.DA2MAX(IFER)) DA2MAX(IFER)=DANG2
C
C---- Branching ratios
      GAM(IFER)=GAM(IFER)+DANG2
      DGAM(IFER)=DGAM(IFER)+DANG2**2
      IF(NTIR.LT.NLMAX) THEN
        NTIR=NTIR+1
        GOTO 20
      ELSE
        DGAM(IFER)=SQRT(DGAM(IFER))/GAM(IFER)
        GAM(IFER)=GAM(IFER)*(EX2MAX(IFER)-XMO)*((4.*PI)**2)/NLMAX
        IF(IFER.LT.6) GAM(IFER)=3.*GAM(IFER)
        GOTO 4
      ENDIF
C
999   CONTINUE
      IDEB=1
      IF(IDEB.EQ.1) THEN
        WRITE(*,1004)
        DO 87 L=1,11
          IF(L.LT.6) THEN
            WRITE(*,1005) CHAGL4(L+ 10),BR(L)
          ELSE
            WRITE(*,1005) CHAFL4(L+  1),BR(L)
          END IF
87      CONTINUE
      END IF
      WRITE(*,1006)
1004  FORMAT(1X,'************* Branching ratios **************')
1005  FORMAT(1X,'*',4X,'CHIP --> ',A4,5X,D15.6,6X,'*')
1006  FORMAT(1X,'*********************************************')
C
      RETURN
998   CONTINUE
      IER= -1
      RETURN
      END
C*DK BSTAND
      SUBROUTINE BSTAND(BR,IFER,IFLA)
C-------------------------------------------------------------
C! Choose the final fermion flavour following Z*--> f fbar
C! branching ratios
C-------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION BR(11)
C
      TEST=0.
      A=RNDM(A)
      DO 4 I=1,11
       TEST=TEST+BR(I)
       IF(A.LE.TEST) THEN
        IFER=I
        IF(I.LE.5) IFLA=500+I
        IF(I.GT.5) IFLA=I+1
        GX= FLOAT(IFER)+.1
        CALL HF1(10001,GX,1.)
        GOTO 5
       ENDIF
 4    CONTINUE
C
 5    RETURN
      END
C*DK CHICHI
      SUBROUTINE CHICHI(ISTA)
C-------------------------------------------------------------------
C! Event Generation
C   Input : ISTA Status flag
C-------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C*CA MISCEL
      REAL*4 ECM,WEI,VRTEX,SDVRT
      COMMON / MISCEL / IST,IDP,ECM,WEI,NEVENT(6)
      COMMON / VERTEX / VRTEX(4),SDVRT(3)
C*CC MISCEL
C*CA GPARAM
      REAL ZM,GZ,S2THEW
      COMMON / PARWEAK / ZM,GZ,S2THEW
      REAL EB,XMCHI
      INTEGER IFL,IFLO,ITYP,ISGN
      COMMON / GPARAM / EB,XMCHI(2),IFL(2),IFLO(2),ITYP,ISGN
C*CC GPARAM
C*CA RADIAT
      COMMON / RADIAT / XK,KEYRAD
C*CC RADIAT
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (L4CHAG=50, L4CHAF=100)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      REAL*4 PARLU1,PMASL2,PWIDL2,CFRLU2,DPARL3,CBRLU3,PARELE,PARTLU
      CHARACTER*4 CHAGL4,CHAFL4
      COMMON / LUDAT1 / MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON / LUDAT2 / KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID),
     &                  KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON / LUDAT3 / DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR),
     &                  KDPLU3(L3KDP)
      COMMON / LUDAT4 / CHAGL4(L4CHAG),CHAFL4(L4CHAF)
      COMMON / LUDATE / MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON / LUJETS / NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C*CC LUNDCOM
C*CA IDPART
      COMMON / IDPART / ICHI,ICHIP
C*CC IDPART
C*CA FORVEC
      COMMON / FORVEC / QINI(5,3),QFIN(5,8)
C*CC FORVEC
C*CA LUCOPY
      COMMON / LUCOPY / PCOPY(200,5),KCOPY(200,2),NCOPY,NLUO(2)
C*CC LUCOPY
      DIMENSION QK(4),QIN(4),QOUT(4)
      REAL*4 RSZ
      DATA SMALL/0.000001/
C
      ISTA = 0
      ECM = 2. * EB
      XK = 0.
C
C--- Photon momentum generation
      IF(KEYRAD.EQ.0) THEN
       QK(1) = 0.
       QK(2) = SMALL
       QK(3) = 0.
       QK(4) = SMALL
      ELSE
       INDEX = 1
       CALL REMT2(QK,INDEX)
      ENDIF
      XK = QK(4) / EB
      IF(XK .LT. 0.) XK = 0.
      CALL HFILL(10002,QK(4),0.,1.)
C
C--- Change the centre of mass energy value
      ECM = ECM * SQRT(1. - XK)
      CALL HFILL(10003,ECM,0.,1.)
C
C--- Initial particles
      QINI(1,1) = 0.
      QINI(2,1) = 0.
      QINI(3,1) = EB
      QINI(4,1) = EB
      QINI(5,1) = PMASL2(7)
      QINI(1,2) = 0.
      QINI(2,2) = 0.
      QINI(3,2) =-EB
      QINI(4,2) = EB
      QINI(5,2) = PMASL2(7)
      QINI(1,3) = QK(1)
      QINI(2,3) = QK(2)
      QINI(3,3) = QK(3)
      QINI(4,3) = QK(4)
      QINI(5,3) = 0.
C
      NPARLU = 0
C
C--- Final state generation
      CALL XXPRIM(ISTA)
      IF(ISTA.NE.0) RETURN
C
C     Set up LUND COMMON BLOCK
C
C     1 ... e+     (beam particle)
C     2 ... e-     (beam particle)
C     3 ... gamma  (initial state radiation)
C     4 ... chi1
C     5 ... chi2
C     6 ... chi    |
C     7 ... f      | chi1 decay products
C     8 ... fbar   |
C     9 ... chi    |
C    10 ... f      | chi2 decay products ( if any )
C    11 ... fbar   |
C
C
C--- Fill the COMMON LUND with initial particles
      NPARLU= 5
      DO 11 I1=1,3
        DO 12 I2=1,5
          PARTLU(I1,I2) = QINI(I2,I1)
 12     CONTINUE
 11   CONTINUE
      DO 13 I1=1,2
        K1= I1+3
        DO 14 I2=1,5
          PARTLU(K1,I2) = QFIN(I2,I1)
 14     CONTINUE
 13   CONTINUE
C
C--- e+ , e-
      KODELU(1,1) = 40000
      KODELU(1,2) = -7
      KODELU(2,1) = 40000
      KODELU(2,2) =  7
C
C--- Initial state radiative photon
      KODELU(3,1) = 00000
      KODELU(3,2) = 1
C
C--- chi1
      KODELU(4,1) = 50000
      KODELU(4,2) = ICHIP
C
C--- chi2
      KODELU(5,1) = 50000*(ITYP-1)
      KODELU(5,2) = ICHIP-2+ITYP
C
      KL0= 0
      DO 100 LTY=1,ITYP
C
C--- chi (from chix)
       KL= NPARLU+1
      KL0= KL
       KODELU(KL,1) = 4 + LTY -1
       KODELU(KL,2) = ICHI
       KC= 3 + 3*(LTY-1)
       DO 54 I2=1,5
        PARTLU(KL,I2) = QFIN(I2,KC)
 54    CONTINUE
       NPARLU= NPARLU+1
C
       IF(IFLO(LTY).LT.500) THEN
C--- Add l-lbar from chix in COMMON LUND
        KTY= NPARLU
        DO 5 I=1,5
         KC1= 4 + 3*(LTY-1)
         PARTLU(KTY+1,I) = QFIN(I,KC1  )
         PARTLU(KTY+2,I) = QFIN(I,KC1+1)
 5      CONTINUE
        KODELU(KTY+1,1) = 4 + LTY - 1
        KODELU(KTY+1,2) = IFLO(LTY)
        KODELU(KTY+2,1) = 4 + LTY - 1
        KODELU(KTY+2,2) =-IFLO(LTY)
        NPARLU=NPARLU+2
        CALL LUEXEC
C
       ELSE
C
        IF(NLUO(LTY).EQ.0) THEN
C--- Add q-qbar in COMMON LUND
         KTY= NPARLU
         DO 6 I=1,5
          KC1= 4 + 3*(LTY-1)
          PARTLU(KTY+1,I) = QFIN(I,KC1  )
          PARTLU(KTY+2,I) = 0.
          PARTLU(KTY+3,I) = QFIN(I,KC1+1)
          PARTLU(KTY+4,I) = 0.
 6       CONTINUE
C--- Fill the color lines
         KODELU(KTY+1,1) = 10000 + 4 + LTY -1
         KODELU(KTY+1,2) = IFLO(LTY)
         KODELU(KTY+2,1) = 70000 + KTY + 1
         KODELU(KTY+2,2) =  1000 + KTY + 1
         KODELU(KTY+3,1) = 4 + LTY - 1
         KODELU(KTY+3,2) =-IFLO(LTY)
         KODELU(KTY+4,1) = 70000 + KTY + 3
         KODELU(KTY+4,2) =  1000 + KTY + 3
         PARTLU(KTY+2,1) = KTY + 3
         PARTLU(KTY+2,2) = KTY + 3
         PARTLU(KTY+4,1) = KTY + 1
         PARTLU(KTY+4,2) = KTY + 1
         NPARL0= NPARLU
         NPARLU = NPARLU+4
C
C--- Generate Parton Shower
         XMI= XMCHI(2)
         XMO= XMCHI(1)
         ZSM= 0.
         DO 17 IZ=1,3
          ZSM= ZSM + QFIN(IZ,LTY)*QFIN(IZ,KC)
 17      CONTINUE
         ZSM= QFIN(4,LTY)*QFIN(4,KC) - ZSM
         ZSM= XMI*XMI+XMO*XMO-2.*ZSM
         RSZ= SQRT(ZSM)
         CALL LUSHOW(KTY+1,KTY+3,RSZ)
         CALL LUEXEC
C--- Give partons correct number for mother line
         KMOTH= 4 + LTY - 1
         DO 45 L1=NPARL0+1,NPARLU
          IF(ABS(KODELU(L1,2)).LT.500) GOTO 45
          KODELU(L1,1)= 10000*(KODELU(L1,1)/10000) + KMOTH
45       CONTINUE
C
        ELSE
C--- Add fragmentation in COMMON LUND
         NCOP= (LTY-1)*NLUO(1)
         KMOTH= 4 + LTY - 1
         DO 7 I=1,NLUO(LTY)
          DO 8 J=1,5
           PARTLU(I+NPARLU,J) = PCOPY(I+NCOP,J)
 8        CONTINUE
          KODELU(I+NPARLU,2) = KCOPY(I+NCOP,2)
          IF(ABS(KCOPY(I+NCOP,2)).GE.500.OR.
     &           KCOPY(I+NCOP,2).EQ.15)  THEN
           KODELU(I+NPARLU,1)=
     &     10000*(KCOPY(I+NCOP,1)/10000) + KMOTH
          ELSE IF(KCOPY(I+NCOP,1).EQ.-1) THEN
           KODELU(I+NPARLU,1)= KMOTH
          ELSE
           KODELU(I+NPARLU,1) = KCOPY(I+NCOP,1)+NPARLU
          ENDIF
          IF(KODELU(I+NPARLU,2).EQ.1) THEN
           IF(KODELU(I+NPARLU,1).EQ.KL0)
     &                 KODELU(I+NPARLU,1)= KMOTH
          END IF
 7       CONTINUE
         NPARLU = NPARLU+NLUO(LTY)
        ENDIF
       ENDIF
 100  CONTINUE
C
      IF(KEYRAD .NE. 0) THEN
C--- Rotations & boosts due to the initial state radiation
       DO 90 I = 4,NPARLU
        DO 91 J = 1,4
         QIN(J) = PARTLU(I,J)
 91     CONTINUE
        CALL REMT3(QIN,QOUT)
        DO 92 J = 1,4
         PARTLU(I,J) = QOUT(J)
 92     CONTINUE
 90    CONTINUE
      ENDIF
C
      MST35 = MSTLU1(35)
      IF(MSTLU1(35).GT.MST35) CALL LULIST(11)
C
      IF(NEVENT(2).LE.2) CALL LULIST(11)
C
      RETURN
      END
C*DK CHIINIT
      SUBROUTINE CHIINIT(TABL)
C-------------------------------------------------------------------
C! Generator initialisation
C    Input :  TABL Event parameters
C-------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C*CA MISCEL
      REAL*4 ECM,WEI,VRTEX,SDVRT
      COMMON / MISCEL / IST,IDP,ECM,WEI,NEVENT(6)
      COMMON / VERTEX / VRTEX(4),SDVRT(3)
C*CC MISCEL
C*CA GPARAM
      REAL ZM,GZ,S2THEW
      COMMON / PARWEAK / ZM,GZ,S2THEW
      REAL EB,XMCHI
      INTEGER IFL,IFLO,ITYP,ISGN
      COMMON / GPARAM / EB,XMCHI(2),IFL(2),IFLO(2),ITYP,ISGN
C*CC GPARAM
C*CA RADIAT
      COMMON / RADIAT / XK,KEYRAD
C*CC RADIAT
C*CA IDPART
      COMMON / IDPART / ICHI,ICHIP
C*CC IDPART
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (L4CHAG=50, L4CHAF=100)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      REAL*4 PARLU1,PMASL2,PWIDL2,CFRLU2,DPARL3,CBRLU3,PARELE,PARTLU
      CHARACTER*4 CHAGL4,CHAFL4
      COMMON / LUDAT1 / MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON / LUDAT2 / KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID),
     &                  KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON / LUDAT3 / DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR),
     &                  KDPLU3(L3KDP)
      COMMON / LUDAT4 / CHAGL4(L4CHAG),CHAFL4(L4CHAF)
      COMMON / LUDATE / MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON / LUJETS / NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C*CC LUNDCOM
      PARAMETER( NTABL= 14 )
      REAL TABL(NTABL)
      EXTERNAL SIGCHI
C
C--- Rename the event parameters
      EB      = TABL(1)
      XMCHI(1)= TABL(2)
      XMCHI(2)= TABL(3)
      ITYP    = NINT(TABL(4))
      ISGN    = NINT(TABL(5))
      IFL(1)  = NINT(TABL(6))
      IFL(2)  = NINT(TABL(7))
      KEYRAD  = NINT(TABL(8))
      ZM      = TABL(9)
      GZ      = TABL(10)
      S2THEW  = TABL(11)
C
C--- Check input parameters
      IF(ITYP.LT.1.OR.ITYP.GT.2) THEN
       PRINT 101, ITYP
 101   FORMAT(2X,'!!! Wrong ITYP value :',I6,'  STOP !')
       STOP
      ENDIF
      DO 15 L=1,2
       IF(ITYP.EQ.1.AND.L.EQ.2) GOTO 15
       IF(IFL(L) .GT. 505 .OR.
     .   (IFL(L) .GT. 12 .AND. IFL(L) .LE. 500) .OR.
     .   (IFL(L) .LT.  7 .AND. IFL(L) .GT. 0) .OR.
     .    IFL(L) .LT. -3) THEN
        PRINT 102, L,IFL(L)
 102    FORMAT(2X,'!!! Wrong ',I2,'e IFL value :',I6,'  STOP !')
        STOP
       ENDIF
 15   CONTINUE
C--- Check production process
      IF ( XMCHI(2) + XMCHI(ITYP) .GT. 2.* EB ) THEN
       PRINT *,'  !!! Production process not allowed : STOP !'
       STOP
      ENDIF
C
C--- Initialisation of REMT (initial state radiation generator)
      IF(KEYRAD.NE.0) THEN
       EBEAM = EB
       STHR  = (XMCHI(2) + XMCHI(ITYP))**2
       INDEX = 1
       CALL REMT1(EBEAM,SIGCHI,STHR,INDEX)
      ELSE
       SS = 4. * EB**2
       SIG0 = SIGCHI(SS)
       PRINT 11, SIG0
 11    FORMAT(52(1H*),/,
     .       '* nonradiative cross section :',D15.6,' *',/,
     .        52(1H*))
      ENDIF
C
C--- SM Parameters
      PARELE(5) = S2THEW
      PARELE(6) = ZM
      PARELE(7) = GZ
C
C---Add of chi & chip in the particle list
      ICHI          = 98
      ICHIP         = 99
      CHAFL4(ICHIP) = 'CHIP'
      KTYPL2(ICHIP) = 0
      PMASL2(ICHIP) = XMCHI(2)
      CHAFL4(ICHI ) = 'CHI '
      KTYPL2(ICHI ) = 0
      PMASL2(ICHI ) = XMCHI(1)
C
C--- Booking
      CALL HBOOK1(10001,' Fermion flavour distr. ',30,0.,30.,0.)
      CALL HBOOK1(10002,' Radiative photon energy',50,0.,1.,0.)
      CALL HBOOK1(10003,' Centre of mass energy  ',50,70.,95.,0.)
      CALL HMINIM(10001,0.)
      CALL HMINIM(10002,0.)
      CALL HMINIM(10003,0.)
C
      RETURN
      END
C*DK CHITER
      SUBROUTINE CHITER
C-----------------------------------------------------------------
C! Close the generator
C-----------------------------------------------------------------
C
      CALL LUEEVT(10,0.)
C--- Histo aspect
      CALL HIDOPT(0,'1EVL')
      CALL HIDOPT(0,'BLAC')
C
      RETURN
      END
C*DK KFLAV
C-------------------------------------------------------------
C-------------------------------------------------------------
      FUNCTION KFLAV(IFER)
C
      K = 0
      IF(IFER.EQ.1.OR.IFER.EQ.4) K=1
      IF(IFER.EQ.2.OR.IFER.EQ.3.OR.IFER.EQ.5) K=2
      IF(IFER.EQ.7.OR.IFER.EQ.9.OR.IFER.EQ.11) K=3
      IF(IFER.EQ.6.OR.IFER.EQ.8.OR.IFER.EQ.10) K=4
      KFLAV = K
C
      RETURN
      END
C*DK FRAG
      SUBROUTINE FRAG(IFER,KFER,ZSTARM,COTEQ)
C-------------------------------------------------------------
C!  Generate the q qbar fragmentation above LUEXEC threshold
C-------------------------------------------------------------
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SPH(3),QF(4)
      REAL*4 ROOTS,TETSPH,PHISPH
C
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (L4CHAG=50, L4CHAF=100)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      REAL*4 PARLU1,PMASL2,PWIDL2,CFRLU2,DPARL3,CBRLU3,PARELE,PARTLU
      CHARACTER*4 CHAGL4,CHAFL4
      COMMON / LUDAT1 / MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON / LUDAT2 / KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID),
     &                  KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON / LUDAT3 / DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR),
     &                  KDPLU3(L3KDP)
      COMMON / LUDAT4 / CHAGL4(L4CHAG),CHAFL4(L4CHAF)
      COMMON / LUDATE / MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON / LUJETS / NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C*CC LUNDCOM
C*CA VECTRS
      PARAMETER (MAXVEC=500)
      COMMON / VECTRS / VECS(4,MAXVEC),NCHN
C*CC VECTRS
C*CA MISCEL
      REAL*4 ECM,WEI,VRTEX,SDVRT
      COMMON / MISCEL / IST,IDP,ECM,WEI,NEVENT(6)
      COMMON / VERTEX / VRTEX(4),SDVRT(3)
C*CC MISCEL
C*CA GPARAM
      REAL ZM,GZ,S2THEW
      COMMON / PARWEAK / ZM,GZ,S2THEW
      REAL EB,XMCHI
      INTEGER IFL,IFLO,ITYP,ISGN
      COMMON / GPARAM / EB,XMCHI(2),IFL(2),IFLO(2),ITYP,ISGN
C*CC GPARAM
C
      QF(1) =  2. / 3.
      QF(2) = -1. / 3.
      QF(3) =  0.
      QF(4) = -1.
      PI=4.*ATAN(1.)
C
      NPARL2 = NPARLU
      IF(ZSTARM.LE.PARELE(10)) GO TO 511
C----Fragmentation du systeme q-qbar dans le repere du centre de masse
      ROOTS = ZSTARM
      CALL LUONIA(IFER,ROOTS)
C
C----Selection des quarks et des gluons
      NCHN=0
      IQMAX=0
      EQMAX=0.
      DO 510 I=NPARL2+1,NPARLU
       IF(ABS(KODELU(I,2)).GE.500) THEN
        NCHN=NCHN+1
        DO 509 J=1,4
         VECS(J,NCHN)=PARTLU(I,J)
 509    CONTINUE
       ENDIF
C----Selection du quark le plus energetique
       IF(ABS(KODELU(I,2)).GT.500) THEN
        IF(PARTLU(I,4).GT.EQMAX) THEN
         IQMAX=I
         EQMAX=PARTLU(I,4)
        ENDIF
       ENDIF
 510  CONTINUE
C
      IF(NCHN.EQ.0) THEN
C----Selection des particules finales
       DO 520 I=NPARL2+1,NPARLU
        IF(KODELU(I,1).LT.10000) THEN
         NCHN=NCHN+1
         DO 521 J=1,4
          VECS(J,NCHN)=PARTLU(I,J)
 521     CONTINUE
        ENDIF
 520   CONTINUE
      ENDIF
C
C-----Calcul de la sphericite
      CALL SPHER(SPH,SVH)
C
C----Rotation aleatoire du systeme autour de l'axe de sphericite
      QSPH=SQRT(SPH(1)**2+SPH(2)**2+SPH(3)**2)
      TETSPH=ACOS(SPH(3)/QSPH)
      CALL LUROBO(TETSPH,0.,0.,0.,0.)
      A=RNDM(A)
      PHISPH=A*2.*PI
      CALL LUROBO(0.,PHISPH,0.,0.,0.)
C
C----Orientation de la sphericite suivant le quark le plus energique
      IF(IQMAX.GT.0) THEN
       COTEQ=PARTLU(IQMAX,3)*KODELU(IQMAX,2)
      ELSE
C----Orientation de la sphericite suivant la charge dans chaque
C     hemisphere
       CHARHP=0.
       DO 525 I=1,NPARLU
        IF(KODELU(I,1).LT.10000.AND.PARTLU(I,3).GT.0)
     &     CHARHP=CHARHP+PLU(I,6)
 525   CONTINUE
       IF(CHARHP.NE.0) THEN
        COTEQ=CHARHP*QF(KFER)
       ELSE
        A=RNDM(A)
        COTEQ=A-0.5
       ENDIF
      ENDIF
      RETURN
C
 511  CONTINUE
C----Generation d'une paire pi+ pi- si ZSTARM < 1 Gev
      NPARLU=2
      KODELU(1,1)= -1
      KODELU(1,2) =  17
      PARTLU(1,1) = 0.
      PARTLU(1,2) = 0.
      PARTLU(1,3) = SQRT((ZSTARM/2.)**2-PMASL2(17)**2)
      PARTLU(1,4) = ZSTARM/2.
      PARTLU(1,5) = PMASL2(17)
      KODELU(2,1)= -1
      KODELU(2,2) = -17
      PARTLU(2,1) = 0.
      PARTLU(2,2) = 0.
      PARTLU(2,3) = -PARTLU(1,3)
      PARTLU(2,4) = ZSTARM/2.
      PARTLU(2,5) = PMASL2(17)
      IF(IFER.EQ.1) COTEQ= 1.
      IF(IFER.EQ.2) COTEQ=-1.
C
      RETURN
      END
C*DK LORENZ
      SUBROUTINE LORENZ(BETAX,BETAY,BETAZ,P)
C-------------------------------------------------------------------
C!  Perform a Lorentz transformation of the quadriimpulsion P
C   from frame 1 to frame 2
C
C   Input:     Passed:    --BETAX,Y,Z    2's velocity / 1
C                         --P,           quadriimpulsion in 1
C
C   Output:    Passed:    --P,           quadriimpulsion in 2
C
C   P. Janot  --  20 oct 1988
C-------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,M-Z)
      DIMENSION P(4)
C
      BETA2 = BETAX**2 + BETAY**2 + BETAZ**2
      IF(BETA2 .EQ. 0.D0) RETURN
      GAMMA = 1.D0/DSQRT(1.D0-BETA2)
      ONE   = BETAX*P(1) + BETAY*P(2) + BETAZ*P(3)
      TWO   = (GAMMA-1.D0)*ONE/BETA2- GAMMA*P(4)
      P(1)  = P(1) + BETAX*TWO
      P(2)  = P(2) + BETAY*TWO
      P(3)  = P(3) + BETAZ*TWO
      P(4)  = GAMMA*(-ONE+P(4))
C
      RETURN
      END
C*DK LUTAUD
      SUBROUTINE LUTAUD(IFL)
C----------------------------------------------------------------------
C B.Bloch-Devaux December 88
C! Dummy version of LUTAUD routine in LUNMOD code
C  If used, standard decays of taus will be done
C
C-----------------------------------------------------------------------
      IFL = 0
      WRITE(6,101)
 101  FORMAT(//,10X,'+++ WARNING - LUTAUD DUMMY VERSION USED !!',
     & ' standard LUND scheme for decay modes and branching ratios',//)
      RETURN
      END
C*DK PREDEC
      SUBROUTINE PREDEC
C-------------------------------------------------------------------
C  Prepare chip decay
C-------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (L4CHAG=50, L4CHAF=100)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      REAL*4 PARLU1,PMASL2,PWIDL2,CFRLU2,DPARL3,CBRLU3,PARELE,PARTLU
      CHARACTER*4 CHAGL4,CHAFL4
      COMMON / LUDAT1 / MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON / LUDAT2 / KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID),
     &                  KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON / LUDAT3 / DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR),
     &                  KDPLU3(L3KDP)
      COMMON / LUDAT4 / CHAGL4(L4CHAG),CHAFL4(L4CHAF)
      COMMON / LUDATE / MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON / LUJETS / NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C*CC LUNDCOM
C*CA MISCEL
      REAL*4 ECM,WEI,VRTEX,SDVRT
      COMMON / MISCEL / IST,IDP,ECM,WEI,NEVENT(6)
      COMMON / VERTEX / VRTEX(4),SDVRT(3)
C*CC MISCEL
C*CA GPARAM
      REAL ZM,GZ,S2THEW
      COMMON / PARWEAK / ZM,GZ,S2THEW
      REAL EB,XMCHI
      INTEGER IFL,IFLO,ITYP,ISGN
      COMMON / GPARAM / EB,XMCHI(2),IFL(2),IFLO(2),ITYP,ISGN
C*CC GPARAM
C*CA CHIDEC
      COMMON / CHIDEC / T3F(4),QF(4),XMF(11),GAM(11),DGAM(11),BR(11),
     +                  EX2MAX(11),DA2MAX(11),AF(4),BF(4),
     +                  DA1MAX,BWMAX,
     +                  EX(2),PX(2)
C*CC CHIDEC
C
      PI=4.*ATAN(1.)
C
C---Reading the event parameters
      XM1 = XMCHI(   2)
      XM2 = XMCHI(ITYP)*ISGN
C
      EX(1)=(ECM*ECM+XM1*XM1-XM2*XM2)/(2.*ECM)
      PX(1)=SQRT(EX(1)*EX(1)-XM1*XM1)
      EX(2)= ECM-EX(1)
      PX(2)=SQRT(EX(2)*EX(2)-XM2*XM2)
C
      DA1MAX= EX(1)*EX(2) + PX(1)*PX(2) - XM1*XM2
C
      XMI= XMCHI(2)
      XMO= XMCHI(1)
      BWMAX=((XMI-XMO)**2-ZM**2)**2+(ZM*GZ)**2
C
      DO 7 IFER = 1,11
        IF (IFER.LT.6) THEN
          XMF(IFER) = PMASL2(IFER+100)
        ELSE
          XMF(IFER) = PMASL2(IFER+1)
        ENDIF
      EX2MAX(IFER)=((XMI**2+XMO**2-4.*(XMF(IFER)**2))/(2.*XMI))
 7    CONTINUE
C
      QF(1)=  2./3.
      QF(2)= -1./3.
      QF(3)=  0.
      QF(4)= -1.
      T3F(1)= .5
      T3F(2)=-.5
      T3F(3)= .5
      T3F(4)=-.5
C
C--- Couplings
      DO 6 KFER  = 1,4
        AF(KFER)=S2THEW*QF(KFER)-T3F(KFER)
        BF(KFER)=QF(KFER)*S2THEW
 6    CONTINUE
C
      RETURN
      END
C*DK PS
      DOUBLE PRECISION FUNCTION PS(P,Q)
C-------------------------------------------------------------
C!  Calculate the scalar product of 4-vectors P and Q
C-------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(4),Q(4)
C
      X=P(4)*Q(4)
      DO 200 I=1,3
       X=X-P(I)*Q(I)
 200  CONTINUE
      PS=X
C
      RETURN
      END
C*DK REMT1
      SUBROUTINE REMT1(EBEAM,CROSS,STHR,INDEX)
C-----------------------------------------------------------------------
C!THIS PART INITIALIZES THE INITIAL-STATE RADIATOR.
C IT CALCULATES SOME QUANTITIES, AND PERFORMS THE
C NUMERICAL INTEGRATION OVER THE PHOTON SPECTRUM.
C EBEAM=BEAM ENERGY (IN GEV)
C CROSS=NONRADIATIVE CROSS SECTION, TO BE DEFINED
C       WITH ONE VARIABLE: CROSS(S),
C       WHERE S IS THE INVARIANT MASS OF THE E+E- PAIR.
C STHR =THE KINEMATICAL THRESHOLD, I.E. THE LOWEST ALLOWED
C       VALUE OF S FOR WHICH THE NONRADIATIVE PROCESS CAN
C       TAKE PLACE ( IN GEV**2 )
C
C
C 851015 A. SCHWARZ. HAVE TO CALL REMT1 ALWAYS WHEN MASS
C        OF Z0-DECAY PRODUCTS CHANGES, SINCE THRESHOLD CHANGES
C        AS WELL; DIMENSION OF X(1000) CHANGED TO X(1000,10)
C
C-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(1000,10),F(1000),A(1000),Y(1000),Z(1000),XNEW(1000)
      DIMENSION QK(4),QIN(4),QOUT(4)
      DATA INIT/0/
      DIMENSION MINDEX(12)
      DATA MINDEX/2,1,3,1,4,1,5,6,7,8,9,10/
C
C DEFINITION OF BREMSSTRAHLUNG SPECTRUM
      SPECTR(XK)=FAC*(1.+(1.-XK)**2)/XK*CROSS(S*(1.-XK))
C        Store EBEAM into local variable EBEA for later use
      EBEA = EBEAM
C INITIALIZE A FEW QUANTITIES AND CONSTANTS
      S  = 4.*EBEAM**2
      XPS= (.511D-03/EBEAM)**2/2.
      XPT= (2.+XPS)/XPS
      XPL= DLOG(XPT)
      PI = 4.*DATAN(1.D0)
      TPI= 2.*PI
      ALF= 1./137.036D0
      FAC= ALF/PI*(XPL-1.)
      XKC= DEXP( - ( PI/(2.*ALF) + 3./4.*XPL + PI**2/6. - 1. )
     .            /( XPL - 1. )                           )
      IF(INIT.GT.0) GO TO 800
      INIT = 1
      PRINT 1,EBEAM,XKC
    1 FORMAT(
     .  2X,'Initialisation of routine REMT :',/,
     .  3X,'Beam energy  : ',F7.3,' GEV',/,
     .  3X,'Minimal bremsstrahlung energy : ',D15.9,' * Ebeam')
  800 CONTINUE
C
C PARAMETERS OF NUMERICAL INTEGRATION STEP
      N    = 500
      ITER = 6
      X1   = XKC
      XN   = 1.-STHR/S
C     PRINT 2,X1,XN,N,ITER
    2 FORMAT('0PARAMETERS OF SPECTRUM INTEGRATION:',/,
     .       '0LOWEST  K VALUE   : ',D10.3,/,
     .       '0HIGHEST K VALUE   : ',D10.3,/,
     .       '0NO. OF POINTS     : ',I5,/,
     .       '0NO. OF ITERATIONS : ',I3)
C
C INITIALIZE BY CHOOSING EQUIDISTANT X VALUES
      IT=0
      M=N-1
      DX=(XN-X1)/DFLOAT(M)
      X(1,INDEX)=X1
      DO 101 I=2,N
  101 X(I,INDEX)=X(I-1,INDEX)+DX
C
C STARTING POINT FOR ITERATIONS
  100 CONTINUE
C
C CALCULATE FUNCTION VALUES
      DO 102 I=1,N
  102 F(I)=SPECTR(X(I,INDEX))
C
C CALCULATE BIN AREAS
      DO 103 I=1,M
  103 A(I)=(X(I+1,INDEX)-X(I,INDEX))*(F(I+1)+F(I))/2.
C
C CALCULATE CUMULATIVE SPECTRUM Y VALUES
      Y(1)=0.D0
      DO 104 I=2,N
  104 Y(I)=Y(I-1)+A(I-1)
C
C PUT EQUIDISTANT POINTS ON Y SCALE
      DZ=Y(N)/DFLOAT(M)
      Z(1)=0.D0
      DO 105 I=2,N
  105 Z(I)=Z(I-1)+DZ
C
C DETERMINE SPACING OF Z POINTS IN BETWEEN Y POINTS
C FROM THIS, DETERMINE NEW X VALUES AND FINALLY REPLACE OLD VALUES
      XNEW(1)=X(1,INDEX)
      XNEW(N)=X(N,INDEX)
      K=1
      DO 108 I=2,M
  106 IF( Y(K+1) .GT. Z(I) ) GOTO 107
      K=K+1
      GOTO 106
  107 R= ( Z(I) - Y(K) ) / ( Y(K+1) - Y(K) )
  108 XNEW(I) = X(K,INDEX) + ( X(K+1,INDEX)-X(K,INDEX) )*R
      DO 109 I=1,N
  109 X(I,INDEX)=XNEW(I)
C
C CHECK ON END OF ITERATIONS AND RETURN
      IT=IT+1
C     PRINT 3,IT,Y(M)
C   3 FORMAT('0ITERATION NO.=',I3,'  INTEGRAL =',D15.6)
      IF(IT.LT.ITER) GOTO 100
C
C PRESENT RESULTS IN FORM OF CORRECTION
      SIG0 = CROSS(S)
      SIG1 = Y(M)
      DELT = (SIG1/SIG0-1.)*100.
      IF(INIT.GT.1) RETURN
      INIT = 2
      PRINT 4,SIG0,SIG1,DELT
    4 FORMAT(2X,'Results of the initialization step :',/,
     .       3X,'Nonradiative cross section :',D15.6,/,
     .       3X,'   Radiative cross section :',D15.6,/,
     .       3X,'   Radiative correction    :',F10.3,' %',/,
     .       ' ',80(1H=))
      RETURN
      ENTRY REMT2(QK,IDEC)
C-----------------------------------------------------------------------
C THIS PART GENERATES A BREMSSTRAHLUNG PHOTON
C AND CALCULATES WHICH BEAM AXIS TO CHOOSE FOR
C THE GENERATION OF THE 'NONRADIATIVE' CROSS SECTION.
C THE PHOTON ENERGY SPECTRUM MUST HAVE BEEN EXAMINED
C BY CALLING ENTRY 'REMT1' BEFORE THE FIRST CALL TO
C THIS ENTRY.
C-----------------------------------------------------------------------
C
C INITIALIZE FLAG FOR REMT3
CVB   INDX = MINDEX(IDEC)
      INDX = IDEC
      IR=0
C
C GENERATE PHOTON ENERGY FROM CUMULATIVE SPECTRUM BINS
      R=M*RNDM(DUM)
      I=IDINT(R)
      S=R-I
      XK = X(I+1,INDX) + S*( X(I+2,INDX)-X(I+1,INDX) )
C
C GENERATE AZIMUTHAL SCATTERING ANGLE OF THE PHOTON
      FG=TPI*RNDM(DUM)
C
C GENERATE COSINE OF POLAR SCATTERING ANGLE OF THE PHOTON
  201 IT=IT+1
      V= XPS * ( XPT**RNDM(DUMM) - 1. )
      W= XPS + V*(1.-.5*V)
      W= RNDM(DUMY)/(1.-(XK*XK*W+2.*XPS*(1.-XK)/W)/(1.+(1.-XK)**2))
      IF(W.GT.1.D0) GOTO 201
      W= -1. + 2.*W
      CG=DSIGN(1.-V,W)
C
C CHOOSE WHICH OF THE TWO Z AXES SHOULD BE CONSIDERED
                                                             CH=-1.
      IF(DABS(W).LT.(1./(1.+(1.-2./(1.+XK*CG/(2.-XK)))**2))) CH=+1.
C
C CONSTRUCT PHOTON FOUR-MOMENTUM
      SG=DSQRT(V*(2.-V))
      QK(4)=XK*EBEA
      QK(1)=QK(4)*SG*DCOS(FG)
      QK(2)=QK(4)*SG*DSIN(FG)
      QK(3)=QK(4)*CG
C
      RETURN
C
      ENTRY REMT3(QIN,QOUT)
C-----------------------------------------------------------------------
C THIS PART PERFORMS THE ROTATIONS AND BOOSTS OF THE I.S.R.
C FORMALISM AFTER THE USER'S BLACK BOX HAS RUN AN EVENT.
C THE INPUT VECTOR (FROM USERS BLACK BOX) IS QIN;
C THE RESULTING VECTOR IN THE LAB FRAME IS QOUT.
C-----------------------------------------------------------------------
C
C INITIALIZATION PART: ONCE FOR EVERY GENERATED PHOTON MOMENTUM
      IF(IR.NE.0) GOTO 301
      IR=1
C
C CALCULATE ROTATTION PARAMETERS FOR BEAM DIRECTION IN C.M.S.
      XKP = DSQRT( QK(1)**2 + QK(2)**2 )
      XKM = 2.* DSQRT( EBEA*(EBEA-QK(4)) )
      XKD = 2.*EBEA - QK(4) + XKM
      XKA = ( CH + QK(3)/XKD )/XKM
      XKB = DSQRT( (1.+XKA*QK(3))**2 + (XKA*XKP)**2 )
      S1  = XKA*XKP/XKB
      C1  = (1.+XKA*QK(3))/XKB
      S2  = QK(1)/XKP
      C2  = QK(2)/XKP
      YK=QK(4)**2-QK(1)**2-QK(2)**2-QK(3)**2
      Y1=C1**2+S1**2-1.
      Y2=C2**2+S2**2-1.
C
C ROTATE INPUT VECTOR QIN(I) TO CORRESPOND WITH CHOZEN Z-AXIS
  301 QQ =  C1*QIN(2) + S1*QIN(3)
      QZ = -S1*QIN(2) + C1*QIN(3)
      QX =  C2*QIN(1) + S2*QQ
      QY = -S2*QIN(1) + C2*QQ
C
C BOOST ROTATED VECTOR TO LAB FRAME VECTOR QOUT
      QOUT(4)=((XKD-XKM)*QIN(4)-QK(1)*QX-QK(2)*QY-QK(3)*QZ)/XKM
      QQ     =(QIN(4)+QOUT(4))/XKD
      QOUT(1)= QX - QK(1)*QQ
      QOUT(2)= QY - QK(2)*QQ
      QOUT(3)= QZ - QK(3)*QQ
C
      RETURN
      END
C*DK ROT3D
      SUBROUTINE ROT3D(CTETA,PHI,P,Q)
C-----------------------------------------------------------------
C! Transforme le vecteur P exprime dans X,Y,Z dans le vecteur Q
C  dans x,y,z
C  en fonction du cos CTETA de l'angle entre z et Z,
C  et de l'angle PHI entre
C     x et X.
C-----------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(4),W(4),Q(4)
C
      DO 1 L=1,4
       W(L)= P(L)
 1    CONTINUE
C
      STETA=SQRT(1.-CTETA*CTETA)
      CPHI=COS(PHI)
      SPHI=SIN(PHI)
C
      Q(1)= W(1)*CTETA*CPHI-W(2)*SPHI+W(3)*STETA*CPHI
      Q(2)= W(1)*CTETA*SPHI+W(2)*CPHI+W(3)*STETA*SPHI
      Q(3)=-W(1)*STETA               +W(3)*CTETA
      Q(4)= W(4)
C
      RETURN
      END
C*DK SIGCHI
      DOUBLE PRECISION FUNCTION SIGCHI(SS)
C----------------------------------------------------------------
C!  Lower order Z--> chi1 chi2 cross section
C    needed for Bremss. photon spectrum
C   Input :  SS  (CM energy)**2
C----------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C*CA MISCEL
      REAL*4 ECM,WEI,VRTEX,SDVRT
      COMMON / MISCEL / IST,IDP,ECM,WEI,NEVENT(6)
      COMMON / VERTEX / VRTEX(4),SDVRT(3)
C*CC MISCEL
C*CA GPARAM
      REAL ZM,GZ,S2THEW
      COMMON / PARWEAK / ZM,GZ,S2THEW
      REAL EB,XMCHI
      INTEGER IFL,IFLO,ITYP,ISGN
      COMMON / GPARAM / EB,XMCHI(2),IFL(2),IFLO(2),ITYP,ISGN
C*CC GPARAM
C
      SIGMA = 0.
      SEUIL = (XMCHI(2) + XMCHI(ITYP))**2
      IF(SS .LE. SEUIL) GO TO 99
      IF(ITYP .EQ. 1) THEN
       PSTAR = SQRT(SS - (XMCHI(1) + XMCHI(2))**2) *
     .         SQRT(SS - (XMCHI(1) - XMCHI(2))**2) /
     .         (2. * SQRT(SS))
       ESTAR1 = (SS + XMCHI(1)**2 - XMCHI(2)**2) /
     .         (2. * SQRT(SS))
       ESTAR2 = (SS + XMCHI(2)**2 - XMCHI(1)**2) /
     .         (2. * SQRT(SS))
       SIG = ESTAR1 * ESTAR2 + PSTAR**2 / 3.
     .       - ISGN * XMCHI(1) * XMCHI(2)
      ELSE
       PSTAR = SQRT(SS - 4. * XMCHI(2)**2) / 2.
       ESTAR = SQRT(SS) / 2.
       SIG = ESTAR**2 + PSTAR**2 /3. - XMCHI(2)**2
      ENDIF
      PROPZ = (SS - ZM**2)**2 + (ZM * GZ)**2
      SIGMA = SIG * PSTAR / SQRT(SS) / PROPZ
 99   CONTINUE
C
      SIGCHI = SIGMA
C
      RETURN
      END
C*DK SPHER
      SUBROUTINE SPHER(SPH,SVH)
C-------------------------------------------------------------
C! Calculate the sphericity axis
C-------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C*CA VECTRS
      PARAMETER (MAXVEC=500)
      COMMON / VECTRS / VECS(4,MAXVEC),NCHN
C*CC VECTRS
      DIMENSION SPH(3)
      REAL*4 SXH(3,3),TH(3,3),EVH(3),WH(3)
C
C*** Initialisation
      SPH(1)=0.
      SPH(2)=0.
      SPH(3)=1.
      SVH=1.
      SXT=0.
      DO 6 I = 1,3
       DO 7 J = 1,3
        SXH(I,J) = 0.
 7     CONTINUE
 6    CONTINUE
      IF(NCHN.LE.0) GO TO 999
C
C*** LOOP OVER ALL PARTICLES
C
      DO 1 N=1,NCHN
       DO 2 I=1,3
        DO 12 J=1,3
         SXH(I,J)=SXH(I,J)+VECS(I,N)*VECS(J,N)
12      CONTINUE
        SXT=SXT+VECS(I,N)**2
2      CONTINUE
1     CONTINUE
C
C*** CALCULATE THE SPHERICITY AXIS
C
      IF(SXT.LE.0.) GO TO 999
      DO 13 I=1,3
       DO 14 J=1,3
        SXH(I,J)=SXH(I,J)/SXT
        TH(I,J) = SXH(I,J)
14     CONTINUE
13    CONTINUE
      CALL EISRS1(3,3,TH,EVH,SXH,IERR,WH)
      DO 15 I = 1,3
       SPH(I) = SXH(I,3)
15    CONTINUE
      SVH=EVH(3)
C
999   CONTINUE
      RETURN
      END
C*DK USCJOB
      SUBROUTINE USCJOB
C --------------------------------------------------------------------
C!End of generation
C --------------------------------------------------------------------
C*CA MISCEL
      REAL*4 ECM,WEI,VRTEX,SDVRT
      COMMON / MISCEL / IST,IDP,ECM,WEI,NEVENT(6)
      COMMON / VERTEX / VRTEX(4),SDVRT(3)
C*CC MISCEL
C
C End of generation
C
      CALL CHITER
C
C Print event counters
C
       WRITE(6   ,101)
  101  FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
       WRITE(6   ,102) NEVENT(1),NEVENT(2),NEVENT(3)
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 in ASKUSE) = ',I10)
       WRITE(6   ,103)
  103  FORMAT(//20X,'ERRORS STATISTICS',
     &         /20X,'*****************')
       WRITE(6   ,104) NEVENT(4),NEVENT(5),NEVENT(6)
  104  FORMAT(/10X,'ISTA # 0 FROM KINMIN        # OF REJECT = ',I10,
     &        /10X,'ISTA # 0 FROM KXLUAL        # OF REJECT = ',I10,
     &        /10X,'ISTA # 0 FROM XXPRIM        # OF REJECT = ',I10)
      RETURN
      END
C*DK X0PDEC
      SUBROUTINE X0PDEC(QIN0,IFLA,QOUT,WEIGHT,IER)
C-------------------------------------------------------------------
C! Chi decay
C-------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (L4CHAG=50, L4CHAF=100)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      REAL*4 PARLU1,PMASL2,PWIDL2,CFRLU2,DPARL3,CBRLU3,PARELE,PARTLU
      CHARACTER*4 CHAGL4,CHAFL4
      COMMON / LUDAT1 / MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON / LUDAT2 / KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID),
     &                  KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON / LUDAT3 / DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR),
     &                  KDPLU3(L3KDP)
      COMMON / LUDAT4 / CHAGL4(L4CHAG),CHAFL4(L4CHAF)
      COMMON / LUDATE / MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON / LUJETS / NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C*CC LUNDCOM
C*CA FORVEC
      COMMON / FORVEC / QINI(5,3),QFIN(5,8)
C*CC FORVEC
C*CA CHIDEC
      COMMON / CHIDEC / T3F(4),QF(4),XMF(11),GAM(11),DGAM(11),BR(11),
     +                  EX2MAX(11),DA2MAX(11),AF(4),BF(4),
     +                  DA1MAX,BWMAX,
     +                  EX(2),PX(2)
C*CC CHIDEC
C*CA MISCEL
      REAL*4 ECM,WEI,VRTEX,SDVRT
      COMMON / MISCEL / IST,IDP,ECM,WEI,NEVENT(6)
      COMMON / VERTEX / VRTEX(4),SDVRT(3)
C*CC MISCEL
C*CA GPARAM
      REAL ZM,GZ,S2THEW
      COMMON / PARWEAK / ZM,GZ,S2THEW
      REAL EB,XMCHI
      INTEGER IFL,IFLO,ITYP,ISGN
      COMMON / GPARAM / EB,XMCHI(2),IFL(2),IFLO(2),ITYP,ISGN
C*CC GPARAM
      PARAMETER( FRCUT= 1. )
      DIMENSION QMUP(4),QMUM(4),QZSTAR(4),QXO(4),QXI(4)
      DIMENSION QIN0(5),QOUT(5,3)
      DIMENSION QWK(4)
      REAL*4 TETAMU,PHIMU,BETAZS,TETA2,FI2,BETAXP,TETA1,FI1
C
      IER= 0
      PI=4.*ATAN(1.)
      XMI= XMCHI(2)
      XMO= XMCHI(1)
      PXI= QIN0(3)
C
C----- Choice of final fermion
      IF(IFLA.EQ.0) THEN
        CALL BSTAND(BR,IFER,IFLA)
        KFER  = KFLAV(IFER)
      ELSEIF(IFLA.EQ.-1) THEN
C------- Reject of neutrinos
 13     CALL BSTAND(BR,IFER,IFLA)
        KFER  = KFLAV(IFER)
        IF(KFER.EQ.3) GOTO 13
      ELSEIF(IFLA.EQ.-2) THEN
C------- Quarks selection
 14     CALL BSTAND(BR,IFER,IFLA)
        KFER = KFLAV(IFER)
        IF(KFER.GT.2) GOTO 14
      ELSEIF(IFLA.EQ.-3) THEN
C------- Leptons selection
 16     CALL BSTAND(BR,IFER,IFLA)
        KFER = KFLAV(IFER)
        IF(KFER.NE.4) GOTO 16
      ELSE
        IF(IFLA.LE.12.AND.IFLA.GT.0) IFER=IFLA-1
        IF(IFLA.GT.500) IFER=IFLA-500
        IF((XMI-XMO).LE.(2.*XMF(IFER)+.0001)) GOTO 998
        KFER = KFLAV(IFER)
      ENDIF
C
      CTETA1= QIN0(1)
      PHI1  = QIN0(2)
C
1     CONTINUE
      A=RNDM(A)
      QXO(4)=(EX2MAX(IFER)-XMO)*A+XMO
      DISTR=SQRT(QXO(4)**2-XMO**2)
      A=RNDM(A)
      IF (DISTR.LT.(EX2MAX(IFER)*A)) GOTO 1
C
C---- Chi2 energy distribution ( chip c.m.s. )
      ZSTARM=SQRT(XMO**2+XMI**2-2.*XMI*QXO(4))
      BREITW=BWMAX/((ZSTARM**2-ZM**2)**2+(ZM*GZ)**2)
      WEIGHT= BREITW
C
C----Tirage de cos TETA2 entre le chiprime et le Z*
C    et de l'angle PHI2
      A=RNDM(A)
      CTETA2=2.*(A-0.5)
      A=RNDM(A)
      PHI2=2.*PI*A
C
C----Tirage de cos TMU entre le Z* et le fermion
C    et de l'angle PHIMU
      A=RNDM(A)
      CTMU=2.*A-1.
      A=RNDM(A)
      PHIMU=2.*PI*A
C
C----4-impulsion des 2 fermions dans le repere du Z*
      QMUP(4)= ZSTARM/2.
      PMUP   = SQRT(QMUP(4)**2-XMF(IFER)**2)
      SMU    = SQRT(1.-CTMU**2)
      QMUP(1)= PMUP*SMU*COS(PHIMU)
      QMUP(2)= PMUP*SMU*SIN(PHIMU)
      QMUP(3)= PMUP*CTMU
C
      QMUM(4)= QMUP(4)
      QMUM(1)=-QMUP(1)
      QMUM(2)=-QMUP(2)
      QMUM(3)=-QMUP(3)
C
C----4-impulsion du Z*
      QZSTAR(4)= XMI-QXO(4)
      QZSTAR(1)= 0.
      QZSTAR(2)= 0.
      QZSTAR(3)= SQRT(QZSTAR(4)**2-ZSTARM**2)
C
C----TSlorentz des 2 fermions dans le repere du chiprime
      BETAX = 0.
      BETAY = 0.
      BETAZ = -QZSTAR(3) / QZSTAR(4)
      CALL LORENZ(BETAX,BETAY,BETAZ,QMUP)
      CALL LORENZ(BETAX,BETAY,BETAZ,QMUM)
C
C----4-impulsion du chiprime
      QXI(1)= 0.
      QXI(2)= 0.
      QXI(3)= PXI
      QXI(4)= SQRT(PXI**2+XMI**2)
C
C----4-impulsion du chi2 dans le repere du chiprime
      PXO   = SQRT(QXO(4)**2-XMO**2)
      STETA2= SQRT(1.-CTETA2**2)
      QXO(1)=-PXO*STETA2*COS(PHI2)
      QXO(2)=-PXO*STETA2*SIN(PHI2)
      QXO(3)=-PXO*CTETA2
C
C----Rotation des 2 fermions dans le repere du chiprime
      CALL ROT3D(CTETA2,PHI2,QMUP,QMUP)
      CALL ROT3D(CTETA2,PHI2,QMUM,QMUM)
C
C----Distribution angulaire du chi2 et des 2 fermions dans
C     le repere du chiprime
      D2=QMUP(4)*PS(QMUM,QXO)+QMUM(4)*PS(QMUP,QXO)
     &         +ISGN*XMO*PS(QMUP,QMUM)
      D2P=QXO(4)+2.*ISGN*XMO
      DANG2=((AF(KFER)**2+BF(KFER)**2)*D2
     &         +2.*(XMF(IFER)**2)*AF(KFER)*BF(KFER)*D2P)
C
      IF(DANG2.GT.DA2MAX(IFER)) THEN
       PRINT *,' !! Warning weight > 1. !! :',DANG2/DA2MAX(IFER)
       DA2MAX(IFER) = DANG2
      ENDIF
      DANG2=DANG2/DA2MAX(IFER)
C
C----Distribution du poids
      WEIGHT=WEIGHT*DANG2
C
C---- Lorentz trans. to lab for fermions and chi
      BETAX = 0.
      BETAY = 0.
      BETAZ = -QXI(3) / QXI(4)
      CALL LORENZ(BETAX,BETAY,BETAZ,QXO)
      CALL LORENZ(BETAX,BETAY,BETAZ,QMUP)
      CALL LORENZ(BETAX,BETAY,BETAZ,QMUM)
C
C----- Rotation in the lab of fermions and chi
      CALL ROT3D(CTETA1,PHI1,QMUP,QMUP)
      CALL ROT3D(CTETA1,PHI1,QMUM,QMUM)
      CALL ROT3D(CTETA1,PHI1,QXO,QXO)
C
      IF(IFER.LT.6.AND.ZSTARM.LT.(2.*XMF(IFER)+FRCUT)) THEN
C----Fragmentation du systeme q-qbar dans leur centre de masse
        COTEQ=0.
        CALL FRAG(IFER,KFER,ZSTARM,COTEQ)
C----Rotation dans le repere du Zstar
        IF(COTEQ.GT.0) THEN
          TETAMU=ACOS(CTMU)
        ELSE
          TETAMU=PI-ACOS(CTMU)
          PHIMU =PHIMU+PI
        ENDIF
        CALL LUROBO(TETAMU,PHIMU,0.,0.,0.)
C
C-----TSLorentz et rotation des jets dans le repere du chiprime
        BETAZS = QZSTAR(3)/QZSTAR(4)
        CALL LUROBO(0.,0.,0.,0.,BETAZS)
        TETA2=ACOS(CTETA2)
        FI2 = PHI2
        CALL LUROBO(TETA2,FI2,0.,0.,0.)
C
C---TSLorentz et rotation des jets dans le labo
        BETAXP=QXI(3)/SQRT(PXI**2+XMI**2)
        CALL LUROBO(0.,0.,0.,0.,BETAXP)
        TETA1=ACOS(CTETA1)
        FI1 = PHI1
        CALL LUROBO(TETA1,FI1,0.,0.,0.)
      END IF
C
C----- Rotation of chip in the lab
      CALL ROT3D(CTETA1,PHI1,QXI,QXI)
C
C------- Fill tables
      DO 27 L=1,4
        QIN0(L)  = QXI(L)
        QOUT(L,1)= QXO(L)
        QOUT(L,2)= QMUP(L)
        QOUT(L,3)= QMUM(L)
27    CONTINUE
C
      QIN0(5)  = XMI
      QOUT(5,1)= XMO
      QOUT(5,2)= XMF(IFER)
      QOUT(5,3)= XMF(IFER)
C
999   CONTINUE
      RETURN
C
998   CONTINUE
      IER= -1
      RETURN
C
      END
C*DK XXPRIM
      SUBROUTINE XXPRIM(ISTA)
C-------------------------------------------------------------------
C! Chi1 chi2 production
C  Output : ISTA Status flag
C-------------------------------------------------------------------
C
      IMPLICIT REAL*8(A-H,O-Z)
C*CA MISCEL
      REAL*4 ECM,WEI,VRTEX,SDVRT
      COMMON / MISCEL / IST,IDP,ECM,WEI,NEVENT(6)
      COMMON / VERTEX / VRTEX(4),SDVRT(3)
C*CC MISCEL
C*CA GPARAM
      REAL ZM,GZ,S2THEW
      COMMON / PARWEAK / ZM,GZ,S2THEW
      REAL EB,XMCHI
      INTEGER IFL,IFLO,ITYP,ISGN
      COMMON / GPARAM / EB,XMCHI(2),IFL(2),IFLO(2),ITYP,ISGN
C*CC GPARAM
C*CA FORVEC
      COMMON / FORVEC / QINI(5,3),QFIN(5,8)
C*CC FORVEC
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (L4CHAG=50, L4CHAF=100)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      REAL*4 PARLU1,PMASL2,PWIDL2,CFRLU2,DPARL3,CBRLU3,PARELE,PARTLU
      CHARACTER*4 CHAGL4,CHAFL4
      COMMON / LUDAT1 / MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON / LUDAT2 / KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID),
     &                  KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON / LUDAT3 / DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR),
     &                  KDPLU3(L3KDP)
      COMMON / LUDAT4 / CHAGL4(L4CHAG),CHAFL4(L4CHAF)
      COMMON / LUDATE / MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON / LUJETS / NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
C*CC LUNDCOM
C*CA CHIDEC
      COMMON / CHIDEC / T3F(4),QF(4),XMF(11),GAM(11),DGAM(11),BR(11),
     +                  EX2MAX(11),DA2MAX(11),AF(4),BF(4),
     +                  DA1MAX,BWMAX,
     +                  EX(2),PX(2)
C*CC CHIDEC
C*CA LUCOPY
      COMMON / LUCOPY / PCOPY(200,5),KCOPY(200,2),NCOPY,NLUO(2)
C*CC LUCOPY
      DIMENSION QIN0(5),QOUT(5,3)
C
      ISTA= 0
C
      PI=4.*ATAN(1.)
      XM1= XMCHI(2)
      XM2= XMCHI(ITYP)*ISGN
C
      IF(NEVENT(1).EQ.0) THEN
C--- Check if production is kinematically allowed
       IF (ECM.LT.(XM1+ABS(XM2))) THEN
 5      WRITE(*,*) ' _XXPRIM_ : final state not allowed'
        STOP
       ENDIF
C
 4     CONTINUE
C--- Prepare chip decay
       CALL PREDEC
C
       IF(IFL(1).LE.0.OR.IFL(2).LE.0) THEN
C--- Branching ratios
        CALL BRADEC(IER)
        IF(IER.EQ.-1) GOTO 5
       ENDIF
C
      ENDIF
C
 10   CONTINUE
      NEVENT(1) = NEVENT(1) + 1
C
C--- Angles between beam and chip
      A=RNDM(A)
      CTETA1=2.*(A-0.5)
      A=RNDM(A)
      PHI1=A*2.*PI
C--- Relative weight
      DANG1=(EX(1)*EX(2)+PX(1)*PX(2)*CTETA1**2-XM1*XM2)/DA1MAX
      WEIGHT=DANG1
C
      NCOPY= 0
      NLUO(1)= 0
      NLUO(2)= 0
      DO 100 L=1,ITYP
C
       QIN0(1)= (-1.)**(L+1) * CTETA1
       QIN0(2)= PHI1 + (L-1)*PI
       QIN0(3)= PX(L)
       IFLA= IFL(L)
       CALL X0PDEC(QIN0,IFLA,QOUT,WEIGH1,IER)
       IF(IER.LT.0) GOTO 5
C
C--- /LUJETS/ to /LUCOPY/
       DO 37 L1=1,NPARLU
        DO 38 L2=1,5
         PCOPY(NCOPY+L1,L2)= PARTLU(L1,L2)
 38     CONTINUE
        KCOPY(NCOPY+L1,1)= KODELU(L1,1)
        KCOPY(NCOPY+L1,2)= KODELU(L1,2)
 37    CONTINUE
       NCOPY= NCOPY+NPARLU
       NLUO(L)= NPARLU
C
C--- Reset /LUJETS/
       DO 41 L1=1,NPARLU
        DO 42 L2=1,5
         PARTLU(L1,L2)= 0.
 42     CONTINUE
        KODELU(L1,1)= 0
        KODELU(L1,2)= 0
 41    CONTINUE
       NPARLU= 0
C
       WEIGHT= WEIGHT*WEIGH1
C--- Fill /FORVEC/
       DO 19 J1=1,5
        DO 191 J2=1,3
         JF= 2+3*(L-1)+J2
         QFIN(J1,JF)= QOUT(J1,J2)
 191    CONTINUE
        QFIN(J1,L)= QIN0(J1)
 19    CONTINUE
C
       IFLO(L)= IFLA
 100  CONTINUE
C
      A= RNDM(A)
      IF(WEIGHT.LT.A) THEN
        NEVENT(6)= NEVENT(6)+1
        GOTO 10
      END IF
C
      IF(ITYP.EQ.1) THEN
       DO 29 J=1,3
        QFIN(J,2)=-QIN0(J)
 29    CONTINUE
       QFIN(4,2)= ECM-QIN0(4)
      ENDIF
      QFIN(5,2)= ABS(XM2)
C
      RETURN
      END
