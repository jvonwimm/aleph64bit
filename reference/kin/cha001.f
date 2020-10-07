From BLOCH@alws.cern.ch Fri Feb 13 15:59:26 2004
Date: Fri, 13 Feb 2004 15:56:07 +0100
From: BLOCH@alws.cern.ch
To: BLOCH@alws.cern.ch

C*HE 12/03/91 17:53:39 C
C*DK ASKUSE
      SUBROUTINE ASKUSE (IDPR,ISTA,NTRK,NVRT,ECMS,WEIT)
C --------------------------------------------------------------------
C!Generation                                         April   1989.
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
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA BCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=50000)
      COMMON / BCS / IW(LBCS )
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C*IF DOC
C*CC BCS
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C*CA FIXPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

C*IF DOC
C*CC FIXPAR
C
C- Count events
      IF((NEVENT(2)+1).GE.IDEB1.AND.
     &   (NEVENT(2)+1).LE.IDEB2)      IDEB= 1
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
      VERTX(1) = RN1*SDVRT(1)
      VERTX(2) = RN2*SDVRT(2)
      VERTX(3) = RN3*SDVRT(3)
      VERTX(4) = 0.
C
C  Event generation
C
      IGEN= 0
1     CONTINUE
      IGEN= IGEN+1
      CALL CHACHA(ISTA)
C
      IF(ISTA.NE.0.AND.IGEN.LE.1000000) THEN
       NEVENT(3) = NEVENT(3) + 1
       GO TO 1
      ENDIF
C
C  Event header informations
      IDPR= ITYP(1)*100 + LDC(1,1)*10 + LDC(1,2)
      IDPR= ITYP(2)*100 + LDC(2,1)*10 + LDC(2,2) + IDPR*1000
      ECMS = ENECM
C
C  Book all banks
C
      CALL KXLUAL(VERTX,ISTA,NVRT,NTRK)
      IF(ISTA.NE.0) THEN
       NEVENT(5) = NEVENT(5) + 1
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
C
C*DK ASKUSI
      SUBROUTINE ASKUSI(IGCOD)
C--------------------------------------------------------------------
C!Initialization                                    19 June 1990
C--------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
C*IF DOC
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
C*IF DOC
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
C*IF DOC
      COMMON /FANFER/ AMFER(6,2)
C*IF DOC
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
C*IF DOC
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*IF DOC
C*CC FANBRA
C*CA FASUSY
      PARAMETER( NSUSY= 17 )
      COMMON /FASUSY/ AMSUS(NSUSY),CHSUS(NSUSY),NASUS(NSUSY)
      CHARACTER*4 NASUS
C*CC FASUSY
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (L4CHAG=50, L4CHAF=100)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)
     &                , KDPLU3(L3KDP)
      COMMON /LUDAT4/   CHAGL4(L4CHAG),CHAFL4(L4CHAF)
      CHARACTER*4 CHAGL4,CHAFL4
      COMMON /LUDATE/  MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON /LUJETS/  NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
      REAL PARLU1,
     &     PMASL2,PWIDL2,CFRLU2,
     &     DPARL3,CBRLU3,
     &     PARELE,PARTLU
C*CC LUNDCOM
C*CA BCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=50000)
      COMMON / BCS / IW(LBCS )
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
      PARAMETER( IGCO = 7006 )
      PARAMETER( LPDEC= 48 )
      INTEGER NODEC(LPDEC)
      INTEGER ALTABL,ALRLEP
      EXTERNAL ALTABL,ALRLEP
C
C  Return generator code
C
      IGCOD=IGCO
      WRITE (IW(6),1000) IGCOD
 1000 FORMAT(30X,78('*'),/,40X,'Welcome to the Chargino  MC CHA001'
     $    ,/,70X,'GENERATOR CODE IS :',I10,/,
     $     40X,'Last date of modification is  ',
     $   ' December 3 , 1991',
     $   /,30X,78('*'),/)
C
C- Job parameters
      LUNOUT= 6
      NINTE= 100
      IPAW= 0
      IDEB= 0
      IDEB1= 0
      IDEB2= 0
      IPRT =0
C
      IDEBU = IW(NAMIND('DEBU'))
      IF(IDEBU.NE.0) THEN
        LUNOUT= IW(IDEBU-2)
        IDEB1= IW(IDEBU+1)
        IDEB2= IW(IDEBU+2)
      END IF
C  Input parameters for the generator
C  To be provided on the GENE card
C
      IGENE = IW(NAMIND('GENE'))
      IF(IGENE.NE.0) THEN
        DO 1 L=1,9
          IF(L.EQ.1.OR.L.GE.7) THEN
            TABL(L)= RW(IGENE+L)
          ELSE
            TABL(L)= FLOAT(IW(IGENE+L))
          END IF
 1      CONTINUE
        IPRT = IW(IGENE+10)
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
C- Decode cards
        CALL DECARD
C  Generator initialisation
        CALL INITIA(IER)
        IF(IER.NE.0) THEN
          WRITE(LUNOUT,*) ' _INITIA_ ',IER
          CALL EXIT
        END IF
C- Check LSP
        LSP= 0
        AMSMI= 1.D16
        DO L1=1,13
          IF(AMSUS(L1).LT.AMSMI) THEN
            AMSMI= AMSUS(L1)
            LSP= L1
          END IF
        END DO
        IF(LSP.LT.14.AND.LSP.NE.10.AND.LSP.NE.7) THEN
        WRITE(LUNOUT,*) ' LSP is NEITHER chi0 NOR sneu'
        WRITE(LUNOUT,*) ' Action taken : STOP '
        CALL EXIT
        END IF
C
C----- Introduce new parameters and particles in LUND
        PARELE(5)= SNGL(SINW2)
        PARELE(6)= SNGL(AMSZ0*XFACT)
        PARELE(7)= SNGL(AGMZ0*XFACT)
C----- Define cha2 and chi0
        LUCH2= 97
        LUNE0= 98
        LUSNU= 99
        CHAFL4(LUCH2)= 'CHA2'
        KTYPL2(LUCH2)= 3
        PMASL2(LUCH2)= SNGL(AMSCH(2)*XFACT)
        CHAFL4(LUNE0)= 'CHI0'
        KTYPL2(LUNE0)= 0
        PMASL2(LUNE0)= SNGL(AMSNE(IMSNE(1))*XFACT)
        CHAFL4(LUSNU)= 'SNEU'
        KTYPL2(LUSNU)= 2
        PMASL2(LUSNU)= SNGL(AMSSN*XFACT)
C
C----- Print susy related quantities
        ICHA= 2
        CALL PRCCBR(ICHA)
C----- Branching ratios
        IOPT= 1
        ICH0= IMSNE(1)
        CALL CHI2BR(IOPT,ICHA,ICH0,IER)
        IF(IOPT.EQ.-1) THEN
          WRITE(LUNOUT,*)
     &    ' _ASKUSI_ :  Time limit reached in CHI2BR'
          CALL EXIT
        END IF
        IF(IER.NE.0) THEN
          WRITE(LUNOUT,*) ' _CHI2BR_ troubles ',IER
          CALL EXIT
        END IF
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
        DO L=1,3
          L1= 9+L
          TABL(L1)= SDVRT(L)
        END DO
C
C  Fill the KPAR bank with the generator parameters
        TABL(13)= SNGL(AMSCH(2))
        TABL(14)= SNGL(AMSNE(IMSNE(1)))
        TABL(15)= SNGL(AMSSN)
        TABL(16)= SNGL(FLDCU(2,1))
        TABL(17)= SNGL(FLDNE(IMSNE(1),1))
        TABL(18)= SNGL(FLDNE(IMSNE(1),2))
        TABL(19)= SNGL(SQRT(FLDNE(IMSNE(1),3)**2
     &                     +FLDNE(IMSNE(1),4)**2))
C
C - Initialize ECM dependent quantities if no scan option required
        NASCE = NAMIND('GSCE')
        JGSCE = IW(NASCE)
        NASCA = NAMIND('GSCL')
        JGSCA = IW(NASCA)
        IF((JGSCE+JGSCA).EQ.0) THEN
           CALL USKRIN(TABL(1))
C
C  Fill the RLEP bank
C
           IEBEAM= NINT(TABL(1)*500.)
           JRLEP= ALRLEP(IEBEAM,'    ',0,0,0)
           CALL PRTABL('RLEP',0)
        END IF
C
      ELSE
        WRITE(LUNOUT,*) ' _ASKUSI_: No GENE card : STOP'
        CALL EXIT
      ENDIF
C
      CALL KXLUPA(IPART,IKLIN)
      IF (IPART.LE.0.OR.IKLIN.LE.0) THEN
       WRITE(LUNOUT,'(1X,''ASKUSI:error in PART or KLIN bank
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
C----- Book histos
      CALL HTABLE(10135,' BR',6,0.,6.,6,0.,6.,0.)
      CALL HBOOK1(20001,' COSTHETA CHI2',100,-1.,1.,0.)
      CALL HBOOK1(20002,' COSTHETA DOWNLIKE FERMION',100,-1.,1.,0.)
C
      RETURN
      END
C
C*DK CALCOU
      SUBROUTINE CALCOU
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C-----
      DENO2= 1.+ AV1V2*AV1V2
      STHEV= AV1V2/DSQRT(DENO2)
      CTHEV= 1.   /DSQRT(DENO2)
      CTHEB= STHEV
      STHEB= CTHEV
      SINW= DSQRT(SINW2)
      COSW= DSQRT(1.-SINW2)
      TANW= SINW/COSW
C
      DO 10 L1=1,2
        DO 11 L2=1,2
          CO1RG(L1,L2)= -FLDCU(L1,1)*FLDCU(L2,1)
          CO1RG(L1,L2)= -FLDCU(L1,2)*FLDCU(L2,2)/2.+ CO1RG(L1,L2)
          CO1RG(L1,L2)=  IDELT(L1,L2)*SINW2        + CO1RG(L1,L2)
C
          CO1LF(L1,L2)= -FLDCV(L1,1)*FLDCV(L2,1)
          CO1LF(L1,L2)= -FLDCV(L1,2)*FLDCV(L2,2)/2.+ CO1LF(L1,L2)
          CO1LF(L1,L2)=  CO1LF(L1,L2)*ETACH(L2)
          CO1LF(L1,L2)=  IDELT(L1,L2)*SINW2        + CO1LF(L1,L2)
11      CONTINUE
10    CONTINUE
C
      DO 15 L1=1,2
        DO 16 L2=1,2
          CCHAX(L1,L2)= CO1LF(L1,L2)-CO1RG(L1,L2)
          CCHVC(L1,L2)= CO1LF(L1,L2)+CO1RG(L1,L2)
16      CONTINUE
15    CONTINUE
C
      CFELF(1)= 2*(  .5            )
      CFELF(2)= 2*( -.5 + SINW2    )
      CFELF(3)= 2*(  .5 - SINW2*2/3)
      CFELF(4)= 2*( -.5 + SINW2*1/3)
      CFERG(1)= 0.D0
      CFERG(2)= 2*SINW2
      CFERG(3)=-4*SINW2/3
      CFERG(4)= 2*SINW2/3
C
C SNU --> 1, SLE --> 2, SUP --> 3, SDW --> 4
      DO 5 L=1,4
        WK1=-FLDNE(L,3)+FLDNE(L,4)*AV1V2
        CWNE(1,L)= ETANE(L)*WK1/2.D0/AMSWU
        WK2= FLDNE(L,3)+FLDNE(L,4)/AV1V2
        CWNE(2,L)= ETANE(L)*WK2/2.D0/AMSWU
        WK3= FLDNE(L,1)-TANW*FLDNE(L,2)
        CWNE(3,L)=-SINW*ETANE(L)*WK3
        CWNE(4,L)= FLDNE(L,2)/2.D0/COSW
5     CONTINUE
C
      DO 6 K=1,4
        DO 7 I=1,2
          WKI1=-(STHEV*FLDNE(K,4)-CTHEV*FLDNE(K,3))*FLDCV(I,2)
          WKI2= (SINW*FLDNE(K,1)+COSW*FLDNE(K,2))*FLDCV(I,1)
          COOLF(K,I)= WKI1/DSQRT(2.D0) + WKI2
          WKI1= (CTHEV*FLDNE(K,4)+STHEV*FLDNE(K,3))*FLDCU(I,2)
          WKI2= (SINW*FLDNE(K,1)+COSW*FLDNE(K,2))*FLDCU(I,1)
          COORG(K,I)= WKI1/DSQRT(2.D0) + WKI2
7       CONTINUE
6     CONTINUE
C
C In all the following formulas, multiplication for ETANE means
C complex conjugation
      AV= STHEV*STHEV - CTHEV*CTHEV
      BV= 2.D0*STHEV*CTHEV
      DO 30 L1=1,4
        DO 31 L2=1,4
          WR1=  FLDNE(L1,3)*FLDNE(L2,3)
          WR1= -FLDNE(L1,4)*FLDNE(L2,4) + WR1
          WR2=  FLDNE(L1,3)*FLDNE(L2,4)
          WR2=  FLDNE(L1,4)*FLDNE(L2,3) + WR2
          CO2LF(L1,L2)=-( WR1*AV + WR2*BV )/2.D0 * ETANE(L2)
C
          CO2RG(L1,L2)= -CO2LF(L1,L2)*ETANE(L1)*ETANE(L2)
31      CONTINUE
30    CONTINUE
C
      DO 35 L1=1,4
        DO 36 L2=1,4
          CNEAX(L1,L2)= CO2LF(L1,L2)-CO2RG(L1,L2)
          CNEVC(L1,L2)= CO2LF(L1,L2)+CO2RG(L1,L2)
36      CONTINUE
35    CONTINUE
C
      DO 70 L1=1,4
        DO 71 L2=1,4
          WK1= (FLDNE(L1,3)*CTHEB+FLDNE(L1,4)*STHEB)*FLDNE(L2,2)
          WK2= (FLDNE(L2,3)*CTHEB+FLDNE(L2,4)*STHEB)*FLDNE(L1,2)
          CQ2(L1,L2)= (WK1+WK2)/2.D0/COSW
C
          WK1= FLDNE(L1,1)*COSW-FLDNE(L1,2)*SINW
          WK1= ( FLDNE(L2,1)*COSW-FLDNE(L2,2)*SINW ) * WK1
          WK2= FLDNE(L1,1)*SINW+FLDNE(L1,2)*COSW
          WK2= ( FLDNE(L2,1)*SINW+FLDNE(L2,2)*COSW ) * WK2
          WK3= FLDNE(L1,3)*CTHEB+FLDNE(L1,4)*STHEB
          WK3= (-FLDNE(L2,3)*STHEB+FLDNE(L2,4)*CTHEB ) * WK3
          WK4= FLDNE(L2,3)*CTHEB+FLDNE(L2,4)*STHEB
          WK4= (-FLDNE(L1,3)*STHEB+FLDNE(L1,4)*CTHEB ) * WK4
          CR2(L1,L2)= AMSCM*(WK2+5.D0*TANW*TANW*WK1/3.D0)
          CR2(L1,L2)= CR2(L1,L2)-AMSMU*(WK3+WK4)
          CR2(L1,L2)= CR2(L1,L2)/2.D0/AMSWU
71      CONTINUE
70    CONTINUE
C
      RETURN
      END
C*DK CALHMS
      SUBROUTINE CALHMS(IER)
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C
      IER= 0
C
      AV2V1= 1./AV1V2
      C2BET= 1.D0 - AV2V1*AV2V1
      C2BET= C2BET/(1.D0 + AV2V1*AV2V1)
C
C- Check the value given for mH10
      AMH10MX= AMSZ0*DABS(C2BET)
      IF(AMH10.GT.AMH10MX) THEN
        AMH10= AMH10MX - .5D0/XFACT
      END IF
C
      WK1= AMSZ0*C2BET/AMH10
      WK1= WK1*WK1-1.D0
      IF(WK1.LT.(-1.D-3)) THEN
        WRITE(*,*) WK1
        IER= 1
        RETURN
      ELSE IF(WK1.LT.(0.D0)) THEN
        WK1= 1.D-6
      END IF
      AMH30= (AMSZ0*AMSZ0-AMH10*AMH10)/WK1
C
      WK1= AMH30 + AMSZ0*AMSZ0
      WK2= 4.D0*AMH30*AMSZ0*AMSZ0*C2BET*C2BET
      WK3= DSQRT(WK1*WK1-WK2)
      AMH20= (WK1+WK3)/2.D0
C
      AMHCH= AMH30+AMSWU*AMSWU
C
      AMH20= DSQRT(AMH20)
      AMH30= DSQRT(AMH30)
      AMHCH= DSQRT(AMHCH)
C
C-- Mixing angle
      T2BET= 2.D0*AV2V1/(1.D0-AV2V1*AV2V1)
      WK5= AMH10*AMH10 + AMH20*AMH20
      WK5= WK5/(AMH30*AMH30 - AMSZ0*AMSZ0)
      T2ALF= T2BET*WK5
      C2ALF= 1.D0/DSQRT(1.D0+T2ALF*T2ALF)
      CALF= DSQRT((1.D0+C2ALF)/2.D0)
      SALF= DSQRT((1.D0-C2ALF)/2.D0)
      IF(T2ALF.LT.(0.D0)) SALF=-SALF
C
      RETURN
      END
C*DK CHACHA
      SUBROUTINE CHACHA(ISTA)
C----------------------------------------------------------------
C! Event generation
C  Output  ISTA =0 if o.k.
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C*CA FORVEC
      COMMON /FORVEC/ QINI(5,3),QFIN(5,8)
C*CC FORVEC
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (L4CHAG=50, L4CHAF=100)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)
     &                , KDPLU3(L3KDP)
      COMMON /LUDAT4/   CHAGL4(L4CHAG),CHAFL4(L4CHAF)
      CHARACTER*4 CHAGL4,CHAFL4
      COMMON /LUDATE/  MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON /LUJETS/  NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
      REAL PARLU1,
     &     PMASL2,PWIDL2,CFRLU2,
     &     DPARL3,CBRLU3,
     &     PARELE,PARTLU
C*CC LUNDCOM
      PARAMETER( ATHRE= 1.5 )
      DIMENSION QK(4),QFFB(5),QWIN(4),QWOU(4)
C
      ISTA= 0
C
      XK= 0.D0
      IF(KEYRAD.EQ.0) THEN
        QK(1)= 0.D0
        QK(2)= 0.D0
        QK(3)= 0.D0
        QK(4)= 0.D0
      ELSE IF(KEYRAD.EQ.1) THEN
        INDEX= 1
        CALL REMT2(QK,INDEX)
        XK= 2.D0*QK(4)/ENECM
      ELSE
        WRITE(LUNOUT,*) ' _CHACHA_ Unknown KEYRAD',KEYRAD
        CALL EXIT
      END IF
C
C---- Colliding particles
      QINI(1,1)= 0.D0
      QINI(2,1)= 0.D0
      QINI(3,1)= DSQRT(ENECM*ENECM/4.-AMFER(1,2)*AMFER(1,2))
      QINI(4,1)= ENECM/2.D0
      QINI(5,1)= AMFER(1,2)
      QINI(1,2)= 0.D0
      QINI(2,2)= 0.D0
      QINI(3,2)=-DSQRT(ENECM*ENECM/4.-AMFER(1,2)*AMFER(1,2))
      QINI(4,2)= ENECM/2.D0
      QINI(5,2)= AMFER(1,2)
      QINI(1,3)= QK(1)
      QINI(2,3)= QK(2)
      QINI(3,3)= QK(3)
      QINI(4,3)= QK(4)
      QINI(5,3)= 0.D0
C
C----- Select decay channel
      CALL CHSELD
C
      SQRTS= ENECM*DSQRT(1.D0-XK)
      CALL CHAEVT(SQRTS,IOK)
      IF(IOK.NE.1) GOTO 98
C
C----- Set up /LUJETS/ common
      NPARLU= 5
      DO 10 L1=1,5
        DO 11 L2=1,3
          PARTLU(L2,L1)= SNGL(QINI(L1,L2))
11      CONTINUE
        DO 12 L2=1,2
          K2= L2+3
          PARTLU(K2,L1)= SNGL(QFIN(L1,L2))
12      CONTINUE
10    CONTINUE
C
C----- e+ & e-
      KODELU(1,1)= 40000
      KODELU(1,2)=-7
      KODELU(2,1)= 40000
      KODELU(2,2)= 7
C----- Radiative photon
      KODELU(3,1)= 0
      KODELU(3,2)= 1
C----- chi2+ & chi2-
      KODELU(4,1)= 50000
      KODELU(4,2)= LUCH2
      KODELU(5,1)= 50000
      KODELU(5,2)=-LUCH2
C----- Decay products
      DO 30 L0=1,2
        IF(ITYP(L0).EQ.10) THEN
          KL= NPARLU+1
          KODELU(KL,1)= 4+L0-1
          KODELU(KL+1,1)= 4+L0-1
          KODELU(KL,2)= LUSNU
          KODELU(KL+1,2)= IFLVOU(LDC(L0,1),1)
          IF(L0.EQ.2) KODELU(KL,2)=-KODELU(KL,2)
          IF(L0.EQ.1) KODELU(KL+1,2)=-KODELU(KL+1,2)
          KC= 4 + 3*(L0-1)
          DO 20 L1=1,5
            PARTLU(KL,L1)= SNGL(QFIN(L1,KC+1))
            PARTLU(KL+1,L1)= SNGL(QFIN(L1,KC))
20        CONTINUE
          NPARLU= NPARLU+2
          GOTO 33
        END IF
        KL= NPARLU+1
        KODELU(KL,1)= 4+L0-1
        KODELU(KL,2)= LUNE0
        KC= 3 + 3*(L0-1)
        DO 31 L1=1,5
          PARTLU(KL,L1)= SNGL(QFIN(L1,KC))
31      CONTINUE
        NPARLU= NPARLU+1
        IF(LDC(L0,1).LE.3) THEN
          IUP= IFLVOU(LDC(L0,1),2)
          IDW= IFLVOU(LDC(L0,1),1)
          IF(L0.EQ.1) IDW=-IDW
          IF(L0.EQ.2) IUP=-IUP
          KTY= NPARLU
          DO 32 L1=1,5
            KC1= 4 + 3*(L0-1)
            PARTLU(KTY+1,L1)= SNGL(QFIN(L1,KC1  ))
            PARTLU(KTY+2,L1)= SNGL(QFIN(L1,KC1+1))
32        CONTINUE
          KODELU(KTY+1,1)= 4+L0-1
          KODELU(KTY+2,1)= 4+L0-1
          KODELU(KTY+1,2)= IUP
          KODELU(KTY+2,2)= IDW
          NPARLU= NPARLU+2
33        CONTINUE
          CALL LUPREP
          IF(MSTLU1(26).NE.0) THEN
           MSTLU1(24)= 0
           MSTLU1(26)= 0
           GOTO 98
          END IF
          CALL LUEXEC
        ELSE
C--------- Mass of the fermion system
          KC1= 4 + 3*(L0-1)
          AMFFB= 0.D0
          DO 40 L1=1,3
            AMFFB= AMFFB + QFIN(L1,KC1)*QFIN(L1,KC1+1)
40        CONTINUE
          AMFFB= QFIN(5,KC1  )*QFIN(5,KC1  ) +
     &           QFIN(5,KC1+1)*QFIN(5,KC1+1) +
     &        2*(QFIN(4,KC1  )*QFIN(4,KC1+1) - AMFFB)
          IF(AMFFB.LT.1.D-6) AMFFB= 1.D-6
          AMFFB= DSQRT(AMFFB)
          IF(AMFFB.GT.(QFIN(5,KC1)+QFIN(5,KC1+1)+ATHRE)) THEN
C----------- LUND fragmentation
            IUP= IFLVOU(LDC(L0,1),1)
            IDW= IFLVOU(LDC(L0,2),2)
            IF(L0.EQ.1) IDW= -IDW
            IF(L0.EQ.2) IUP= -IUP
            KTY= NPARLU
            DO 42 L1=1,5
              PARTLU(KTY+1,L1)= SNGL(QFIN(L1,KC1))
              PARTLU(KTY+2,L1)= 0.
              PARTLU(KTY+3,L1)= SNGL(QFIN(L1,KC1+1))
              PARTLU(KTY+4,L1)= 0.
42          CONTINUE
C----------- Fill color lines
            KODELU(KTY+1,1)= 10003 + L0
            KODELU(KTY+1,2)= IUP
            KODELU(KTY+2,1)= 70001 + KTY
            KODELU(KTY+2,2)=  1001 + KTY
            KODELU(KTY+3,1)=     3 + KTY
            KODELU(KTY+3,2)= IDW
            KODELU(KTY+4,1)= 70003 + KTY
            KODELU(KTY+4,2)=  1003 + KTY
            PARTLU(KTY+2,1)=     3 + KTY
            PARTLU(KTY+2,2)=     3 + KTY
            PARTLU(KTY+4,1)=     1 + KTY
            PARTLU(KTY+4,2)=     1 + KTY
            NPARL0= NPARLU
            NPARLU= NPARLU + 4
C----------- Generate parton shower
            CALL LUSHOW(KTY+1,KTY+3,AMFFB)
            CALL LUPREP
            IF(MSTLU1(26).NE.0) THEN
             MSTLU1(24)= 0
             MSTLU1(26)= 0
             GOTO 98
            END IF
            CALL LUEXEC
C----------- Give partons correct number for mother line
            KMOTH= 4 + L0 -1
            DO 45 L1=NPARL0+1,NPARLU
              IF(ABS(KODELU(L1,2)).LT.500) GOTO 45
              KODELU(L1,1)= 10000*(KODELU(L1,1)/10000) + KMOTH
45          CONTINUE
          ELSE
C----------- Fermion system 4-vector
            QFFB(5)= AMFFB
            QFFB(4)= QFIN(4,KC1)+QFIN(4,KC1+1)
            QFFB(3)= DSQRT(QFFB(4)*QFFB(4)-AMFFB*AMFFB)
            QWY= QFIN(2,KC1)+QFIN(2,KC1+1)
            QWX= QFIN(1,KC1)+QFIN(1,KC1+1)
            QFFB(2)= DATAN2(QWY,QWX)
            IF(QFFB(2).LT.(.0)) QFFB(2)= QFFB(2)+2.*APIGR
            QFFB(1)= (QFIN(3,KC1)+QFIN(3,KC1+1))/QFFB(3)
C----------- Two body final states
            CALL TWOMES(L0,LDC(L0,1),LDC(L0,2),QFFB)
            CALL LUPREP
            IF(MSTLU1(26).NE.0) THEN
             MSTLU1(24)= 0
             MSTLU1(26)= 0
             GOTO 98
            END IF
C
            CALL LUEXEC
          END IF
        END IF
30    CONTINUE
179   FORMAT(//,8(1X,5(3X,F9.3),/))
180   FORMAT(//,4I6)
C
      IF(KEYRAD.EQ.1) THEN
C----- Rotations and boosts due to initial state radiation
        DO 50 L1=4,NPARLU
          DO 51 L2=1,4
            QWIN(L2)= PARTLU(L1,L2)
51        CONTINUE
          CALL REMT3(QWIN,QWOU)
          DO 52 L2=1,4
            PARTLU(L1,L2)= QWOU(L2)
52        CONTINUE
50      CONTINUE
      END IF
C
      IF(NEVENT(2).LE.2) CALL LULIST(11)
C
C
      IF(ISTA.EQ.0) RETURN
98    CONTINUE
      ISTA= 1
      RETURN
      END
C*DK CHAEVT
      SUBROUTINE CHAEVT(SQRTS,IOK)
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C*CA FORVEC
      COMMON /FORVEC/ QINI(5,3),QFIN(5,8)
C*CC FORVEC
      PARAMETER( NRANMX= 1000 )
      REAL R1,R2
      DIMENSION QIN0(5),QOUT(5,3)
C
      IOK= 0
      IRAN= 0
1     CONTINUE
      IF(IRAN.GT.NRANMX) GOTO 999
      R1= RNDM(R1)
      IRAN= IRAN + 1
      NEVENT(1)= NEVENT(1)+1
      CTH= -1.D0 + R1*2.D0
      YW= XSPROD(SQRTS,CTH,2,2,4,ISTA)
      IF(ISTA.NE.0) GOTO 1
      R2= RNDM(R2)
      IF(R2.GT.(YW/YMAX)) THEN
        NEVENT(6)= NEVENT(6)+1
        GOTO 1
      END IF
      PHI= RNDM(R2)
      PHI= 2.*APIGR*PHI
C - histo
      CALL HF1(20001,CTH,1.)
C
      APCHI= SQRTS*SQRTS/4.-AMSCH(2)*AMSCH(2)
      IF(APCHI.LT.(1.D-6)) THEN
        APCHI= 0.D0
        IOK= -1
        GOTO 999
      ELSE
        APCHI= DSQRT(APCHI)
      END IF
      DO 10 L1=1,2
        QIN0(1)= (-1.)**(L1+1) * CTH
        QIN0(2)= PHI + (L1-1) * APIGR
        QIN0(3)= APCHI
        QIN0(4)= SQRTS/2.
        QIN0(5)= AMSCH(2)
        LD1= LDC(L1,1)
        LD2= LDC(L1,2)
        CALL CHDECA(QIN0,LD1,LD2,L1,QOUT,IER)
        IF(IER.GE.2) THEN
          WRITE(LUNOUT,*)
     &    ' _CHAEVT_: decays not yet incorporated',IER
          CALL EXIT
        END IF
C------- Fill QFIN
        DO 11 L2=1,5
          DO 111 L3=1,3
            LF= 2 + 3*(L1-1) + L3
            QFIN(L2,LF)= QOUT(L2,L3)
111       CONTINUE
          QFIN(L2,L1)= QIN0(L2)
11      CONTINUE
10    CONTINUE
179   FORMAT(//,4(3X,5(2X,F9.3),/))
C
      IOK= 1
C
999   CONTINUE
      RETURN
      END
C*DK CHASIG
      DOUBLE PRECISION FUNCTION CHASIG(SS)
C----------------------------------------------------------------
C! Cross section (nb)
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C
      NXST= 100
      SQRTS= DSQRT(SS)
      CTHMAX= 1.D0
      ICHA= 2
      ITER= 4
      XS= XSTOT(SQRTS,ICHA,ICHA,ITER,ISTAT)
C
      CHASIG= .389D6*XS
C
      RETURN
      END
C*DK CHDEC2
      SUBROUTINE CHDEC2(QIN0,LD0,QOUT)
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*IF DOC
C*CC FANDCC
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C*CA MASSES
      COMMON /MASSES/ XM1,XM2,XM3
C*CC MASSES
      PARAMETER( NRANMX= 100000 )
      DIMENSION QIN0(5),QOUT(5,3)
      DIMENSION QW1(5),QW2(5),QW3(5),QCHI(5),QS(5)
      REAL R1,R2
C
      PARAMETER( ASAFE= .01 )
      ITWO= 0
      LD1= LDC(LD0,1)
      LD2= LDC(LD0,2)
      IF(AMSSN.LT.AMSNE(IMSNE(1))) THEN
        IF(LD1.LE.3) THEN
          DO L1=1,5
            QW3(L1)= 0.D0
          END DO
          ITWO= 1
        END IF
      END IF
      ITY0= ITYP(LD0)
      IF(ITY0.EQ.2) THEN
        QW1(5)= AMFER(LD1,2)
        QS(5)= AMSSN
        IF(LD1.GE.4) QS(5)= AMSUL
        IF(ITWO.EQ.0) THEN
          QW2(5)= AMFER(LD2,1)
          QW3(5)= AMSNE(IMSNE(1))
        END IF
      ELSE IF(ITY0.EQ.3) THEN
        QW1(5)= AMFER(LD1,1)
        QS(5)= AMSLL
        IF(LD1.GE.4) QS(5)= AMSDL
        QW2(5)= AMFER(LD2,2)
        QW3(5)= AMSNE(IMSNE(1))
      END IF
C
      IF(ITWO.EQ.0) THEN
        IF((QIN0(5).LT.(QW1(5)+QS(5)+ASAFE/XFACT)) .OR.
     &    (QS(5).LT.(QW2(5)+QW3(5)+ASAFE/XFACT))) THEN
          ITYP(LD0)= -1
          RETURN
        END IF
      ELSE IF(ITWO.EQ.1) THEN
        IF(QIN0(5).LT.(QW1(5)+QS(5))) THEN
          ITYP(LD0)= -1
          RETURN
        END IF
      END IF
C
      QW1(4)= AMSCH(2)*AMSCH(2)+QW1(5)*QW1(5)-QS(5)*QS(5)
      QW1(4)= QW1(4)/2./AMSCH(2)
      QS(4)= AMSCH(2)-QW1(4)
      APS= QS(4)*QS(4)-QS(5)*QS(5)
      IF(APS.LT.(1.D-8)) THEN
        ITYP(LD0)= -1
        RETURN
      ELSE
        APS= DSQRT(APS)
      END IF
      QW1(1)= 0.D0
      QW1(2)= APS
      QW1(3)= 0.D0
      QS(1)= 0.D0
      QS(2)=-APS
      QS(3)= 0.D0
      IF(ITWO.EQ.1) THEN
        DO L1=1,5
          QW2(L1)= QS(L1)
        END DO
        GOTO 10
      END IF
C----- S decay within its mass system
      CT1= RNDM(R1)*2.-1.
      ST1= DSQRT(1.-CT1*CT1)
      PH1= 2.*APIGR*RNDM(PH1)
      QW2(4)= QS(5)*QS(5)+QW2(5)*QW2(5)-QW3(5)*QW3(5)
      QW2(4)= QW2(4)/2./QS(5)
      AP2= QW2(4)*QW2(4)-QW2(5)*QW2(5)
      IF(AP2.LT.(1.D-8)) THEN
        ITYP(LD0)= -2
        RETURN
      ELSE
        AP2= DSQRT(AP2)
      END IF
      QW2(1)= AP2*ST1*DCOS(PH1)
      QW2(2)= AP2*ST1*DSIN(PH1)
      QW2(3)= AP2*CT1
      QW3(1)=-QW2(1)
      QW3(2)=-QW2(2)
      QW3(3)=-QW2(3)
      QW3(4)= QS(5)-QW2(4)
C----- Lorentz transf to chi+ cms
      AMS= QS(5)
      PAX=-APS
      CALL LORAX(2,PAX,AMS,QW2)
      CALL LORAX(2,PAX,AMS,QW3)
10    CONTINUE
C----- Rotation : z axes
      CT2= RNDM(R2)*2. -  1.
      TH2= DACOS(CT2)
      CALL ROTAX(3,TH2,QW1)
      CALL ROTAX(3,TH2,QW2)
      IF(ITWO.EQ.0) CALL ROTAX(3,TH2,QW3)
      PH2= RNDM(R2)*2.*APIGR
      CALL ROTAX(2,PH2,QW1)
      CALL ROTAX(2,PH2,QW2)
      IF(ITWO.EQ.0) CALL ROTAX(2,PH2,QW3)
C----- Momenta are now in chargino cm system
      AMS= QIN0(5)
      PAX= QIN0(3)
      CALL LORAX(2,PAX,AMS,QW1)
      CALL LORAX(2,PAX,AMS,QW2)
      IF(ITWO.EQ.0) CALL LORAX(2,PAX,AMS,QW3)
C----- Chargino was along  y axes: give it its direction
      CTH= QIN0(1)
      THE= DACOS(CTH)
      PHI= QIN0(2)
      TH1= APIGR/2.-THE
      CALL ROTAX(1,TH1,QW1)
      CALL ROTAX(1,TH1,QW2)
      IF(ITWO.EQ.0) CALL ROTAX(1,TH1,QW3)
C
      PH1= PHI-APIGR/2.
      CALL ROTAX(3,PH1,QW1)
      CALL ROTAX(3,PH1,QW2)
      IF(ITWO.EQ.0) CALL ROTAX(3,PH1,QW3)
C
      QIN0(1)= QIN0(3)*SIN(THE)*COS(PHI)
      QIN0(2)= QIN0(3)*SIN(THE)*SIN(PHI)
      QIN0(3)= QIN0(3)*CTH
C - HISTO
      AQW1= SQRT(QW1(1)**2+QW1(2)**2+QW1(3)**2)
      CTH1= QW1(3)/AQW1
      CALL HF1(20002,CTH1,1.)
C
C----- Fill QOUT with decay products
      DO 50 L1=1,5
        QOUT(L1,1)= QW3(L1)
        QOUT(L1,2)= QW1(L1)
        QOUT(L1,3)= QW2(L1)
50    CONTINUE
      IF(ITWO.EQ.1) ITYP(LD0)= 10
C
      RETURN
      END
C*DK CHDEC3
      SUBROUTINE CHDEC3(QIN0,LD0,QOUT,IER)
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C*CA MASSES
      COMMON /MASSES/ XM1,XM2,XM3
C*CC MASSES
      PARAMETER( NRANMX= 100000 )
      DIMENSION QIN0(5),QOUT(5,3)
      DIMENSION QW1(5),QW2(5),QW3(5),QCHI(5)
      REAL R1,R2
C
      IER= 0
      LD1= LDC(LD0,1)
      LD2= LDC(LD0,2)
C----- Prepare decays
      ICHA= 2
      ICH0= IMSNE(1)
      IOPT= 1
      CALL CHDECS(IOPT,LD1,LD2,ICHA,ICH0,IER)
      IF(IER.NE.0) THEN
        WRITE(LUNOUT,*) ' _CHDECS_ Decay channel undefined or
     &                    not enough energy to decay',IER
        CALL EXIT
      END IF
C
      QW1(5)= AMFER(LD1,1)
      QW2(5)= AMFER(LD2,2)
      QW3(5)= AMSNE(ICH0)
C
C----- Energies of fermions
40    CONTINUE
      CALL FERENE(QIN0(5),QW3(5),LD1,LD2,X1,X2,C12,IER)
      IF(IER.NE.0) THEN
        WRITE(LUNOUT,*) ' _FERENE_ Cannot decay'
        CALL EXIT
      END IF
C
C----- C.M.S. energies
      QW1(4)= X2
      QW2(4)= X1
      QW3(4)= QIN0(5)-QW1(4)-QW2(4)
C----- C.M.S. momenta
      CALL GETMOM(QIN0,C12,QW1,QW2,QW3,IER)
      IF(IER.GT.0) THEN
        WRITE(LUNOUT,*) ' _GETMOM_ NOT enough energy',IER
        CALL EXIT
      ELSE IF(IER.LT.0) THEN
        GOTO 40
      END IF
C----- First rotation : y axes
      PH1= RNDM(R1)*2.*APIGR
      CALL ROTAX(2,PH1,QW1)
      CALL ROTAX(2,PH1,QW2)
      CALL ROTAX(2,PH1,QW3)
C----- Second rotation : z axes
      CT2= RNDM(R2)*2. -  1.
      TH2= DACOS(CT2)
      CALL ROTAX(3,TH2,QW1)
      CALL ROTAX(3,TH2,QW2)
      CALL ROTAX(3,TH2,QW3)
C----- Momenta are now in chargino cm system
      AMS= QIN0(5)
      PAX= QIN0(3)
      CALL LORAX(2,PAX,AMS,QW1)
      CALL LORAX(2,PAX,AMS,QW2)
      CALL LORAX(2,PAX,AMS,QW3)
C----- Chargino was along  y axes: give it its direction
      CTH= QIN0(1)
      THE= DACOS(CTH)
      PHI= QIN0(2)
      TH1= APIGR/2.-THE
      CALL ROTAX(1,TH1,QW1)
      CALL ROTAX(1,TH1,QW2)
      CALL ROTAX(1,TH1,QW3)
C
      PH1= PHI-APIGR/2.
      CALL ROTAX(3,PH1,QW1)
      CALL ROTAX(3,PH1,QW2)
      CALL ROTAX(3,PH1,QW3)
C
      QIN0(1)= QIN0(3)*SIN(THE)*COS(PHI)
      QIN0(2)= QIN0(3)*SIN(THE)*SIN(PHI)
      QIN0(3)= QIN0(3)*CTH
C
C----- Fill QOUT with decay products
      DO 50 L1=1,5
        QOUT(L1,1)= QW3(L1)
        QOUT(L1,2)= QW1(L1)
        QOUT(L1,3)= QW2(L1)
50    CONTINUE
C
      RETURN
      END
C*DK CHDECA
      SUBROUTINE CHDECA(QIN0,LD1,LD2,LD0,QOUT,IER)
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
      DIMENSION QIN0(5),QOUT(5,3)
C
      IER= 0
C
      IF(ITYP(LD0).EQ.1) THEN
        CALL CHDEC3(QIN0,LD0,QOUT,IER)
      ELSE IF(ITYP(LD0).EQ.2.OR.ITYP(LD0).EQ.3) THEN
        CALL CHDEC2(QIN0,LD0,QOUT)
        IF(ITYP(LD0).LT.0) IER= 2
      ELSE
        IER= 3
      END IF
C
999   CONTINUE
      RETURN
      END
C*DK CHDECS
      SUBROUTINE CHDECS(IOP,LD1,LD2,NCH,N0,IER)
C-------------------------------------------------------------------
C IOP = 1   CHI+ --> CHI0 X
C IOP = 2   CHI0 --> CHI+ X
C Output  IOP=-1 means "Not enough time to do the job"
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C*CA MASSES
      COMMON /MASSES/ XM1,XM2,XM3
C*CC MASSES
      DIMENSION FTHRE(6,6)
      DATA FTHRE /.01,.0 ,.0 ,.0 ,.0 ,.0 ,
     &            .0 ,.01,.0 ,.0 ,.0 ,.0 ,
     &            .0 ,.0 ,.01,.0 ,.0 ,.0 ,
     &            .0 ,.0 ,.0 ,.01,.01,.2 ,
     &            .0 ,.0 ,.0 ,.3 ,.5 ,1. ,
     &            .0 ,.0 ,.0 ,5. ,5. ,5. /
C
      IER= 0
      DO L1=1,6
        DO L2=1,6
          FTHRE(L1,L2)= FTHRE(L1,L2)/XFACT
        END DO
      END DO
C
C- Masses
      IF(IOP.EQ.1) THEN
        AM0= AMSCH(NCH)
        AM3= AMSNE(N0 )
      ELSE IF(IOP.EQ.2) THEN
        AM3= AMSCH(NCH)
        AM0= AMSNE(N0 )
      ELSE
        IER= 1
        RETURN
      END IF
C
      AM1= AMFER(LD2,2)
      AM2= AMFER(LD1,1)
      XM1= AM1/AM0
      XM2= AM2/AM0
      XM3= AM3/AM0
      IF(AM0.LE.(AM1+AM2+AM3+FTHRE(LD1,LD2))) THEN
        IER= 2
        RETURN
      END IF
C- Define constants
      AS= AM0*(AM0*AM0-AM3*AM3)/2
      BS= AM0*AM0
      CS= AM0*AM0-AM3*AM3
      DS= 2*AM0
      FS= AM0*AM3
C
C- Compute effective couplings
      CALL CNRX(LD1,1)
      CN0= CNFR0(1,N0)
      CALL CNRX(LD2,2)
      CN1= CNFR1(1,N0)
      ECP(1,1)= COOLF(N0,NCH)*COOLF(N0,NCH)
      ECP(2,1)= COORG(N0,NCH)*COORG(N0,NCH)
      ECP(3,1)= COOLF(N0,NCH)*COORG(N0,NCH)
      ECP(4,1)= FLDCU(NCH,1)*FLDCU(NCH,1)*CN1*CN1
      ECP(5,1)= FLDCV(NCH,1)*FLDCV(NCH,1)*CN0*CN0
      ECP(6,1)= COOLF(N0,NCH)*FLDCV(NCH,1)*CN0*ETACH(NCH)
      ECP(7,1)= COORG(N0,NCH)*FLDCV(NCH,1)*CN0*ETACH(NCH)
      ECP(8,1)= COOLF(N0,NCH)*FLDCU(NCH,1)*CN1
      ECP(9,1)= COORG(N0,NCH)*FLDCU(NCH,1)*CN1
      ECP(10,1)= FLDCU(NCH,1)*FLDCV(NCH,1)*CN1*CN0
      ECP(10,1)= ECP(10,1)*ETACH(NCH)
C
      ALIM= (AM0-AM3*AM3/AM0)/2.
      AMDEC(1)= AM0
      AMDEC(2)= AM1
      AMDEC(3)= AM2
      AMDEC(4)= AM3
C
      RETURN
      END
C*DK CHI2BR
      SUBROUTINE CHI2BR(IOP,NCH,N0,IER)
C-----------------------------------------------------------------
C     IOP=
C     IER =   0  if o.k.
C
C-----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
C
      DIMENSION BJJ(12),BEJJ(12),RTOP(12)
      DIMENSION UKM(3,3),I2BOD(6,6)
      DATA UKM /.975,.22,.01,.22,.974,.045,
     &          .007,.045,.998 /
C
C- Reset common blocks
      IER= 0
      DO 20 L1=1,6
        DO 21 L2=1,6
          GAMF(L1,L2)= 0.D0
          XSUR(L1,L2)= 0.D0
          XBRF(L1,L2)= 0.D0
          I2BOD(L1,L2)= 0
          DO 22 L3=1,2
            GA2F(L1,L2,L3)= 0.D0
            XBR2(L1,L2,L3)= 0.D0
22        CONTINUE
21      CONTINUE
20    CONTINUE
      IF(IOP.EQ.2) GOTO 49
C
C- Compute partial widths ( 2-body decays )
      ITWO= 0
      GA2= 0.D0
      DO 30 LD1=1,3
        AMF= AMFER(LD1,2)
        AMS= AMSSN
        CR= FLDCV(NCH,1)
        CL= 0.D0
        WK= TWOBOD(AMSCH(NCH),AMF,AMS,CR,CL)
        IF(WK.GT.(0.D0)) I2BOD(LD1,LD1)= 1
        GA2F(LD1,LD1,1)= WK*XFACT
        GA2= GA2 + WK*XFACT
        AMF= AMFER(LD1,1)
        AMS= AMSLL
        CR= FLDCU(NCH,1)
        CL= 0.D0
        WK= TWOBOD(AMSCH(NCH),AMF,AMS,CR,CL)
        IF(WK.GT.(0.D0)) I2BOD(LD1,LD1)= 1
        GA2F(LD1,LD1,2)= WK*XFACT
        GA2= GA2 + WK*XFACT
        IF(I2BOD(LD1,LD1).EQ.1) ITWO= 1
30    CONTINUE
      DO 31 LD1=4,6
        DO 32 LD2=4,6
          LKM1= LD1-3
          LKM2= LD2-3
          AMF= AMFER(LD1,2)
          AMS= AMSUL
          CR= FLDCV(NCH,1)
          CL= 0.D0
          WK= TWOBOD(AMSCH(NCH),AMF,AMS,CR,CL)
          IF(WK.GT.(0.D0)) I2BOD(LD1,LD2)= 1
          WK= UKM(LKM1,LKM2)*UKM(LKM1,LKM2)*WK
          GA2F(LD1,LD2,1)= 3.D0*WK*XFACT
          GA2= GA2 + 3.D0*WK*XFACT
          AMF= AMFER(LD1,1)
          AMS= AMSDL
          CR= FLDCV(NCH,1)
          CL= 0.D0
          WK= TWOBOD(AMSCH(NCH),AMF,AMS,CR,CL)
          IF(WK.GT.(0.D0)) I2BOD(LD1,LD2)= 1
          WK= UKM(LKM1,LKM2)*UKM(LKM1,LKM2)*WK
          GA2F(LD1,LD2,2)= 3.D0*WK*XFACT
          GA2= GA2 + 3.D0*WK*XFACT
          IF(I2BOD(LD1,LD1).EQ.1) ITWO= 1
32      CONTINUE
31    CONTINUE
C- Compute partial widths ( 3-body decays )
49    CONTINUE
      NBRA= 0
      GAM= 0.D0
      DO 50 LD1=1,3
        IF(I2BOD(LD1,LD1).EQ.1) GOTO 50
        LT= 0
        WK= SWA(IOP,LT,LD1,LD1,NCH,N0)*XFACT
        IF(IOP.EQ.-1) RETURN
        IF(LT.LT.0) GOTO 50
        GAM= GAM + WK
        GAMF(LD1,LD1)= WK
        IF(WK.GT.(0.D0)) NBRA= NBRA + 1
50    CONTINUE
      DO 51 LD1=4,6
        DO 52 LD2=4,6
          IF(I2BOD(LD1,LD2).EQ.1) GOTO 52
          LT= 0
          WK= 3.D0*SWA(IOP,LT,LD1,LD2,NCH,N0)*XFACT
          IF(IOP.EQ.-1) RETURN
          IF(LT.LT.0) GOTO 52
          LKM1= LD1-3
          LKM2= LD2-3
          WK= UKM(LKM1,LKM2)*UKM(LKM1,LKM2)*WK
          GAM= GAM + WK
          GAMF(LD1,LD2)= WK
          IF(WK.GT.(0.D0)) NBRA= NBRA + 1
52      CONTINUE
51    CONTINUE
C
      IF(GAM.LT.(-1.D-15).OR.GA2.LT.(-1.D-15)) THEN
        IER= 1
        RETURN
      ELSE IF(DABS(GAM).LT.(1.D-16).AND.
     &        DABS(GA2).LT.(1.D-16)) THEN
        IER= 2
        RETURN
      END IF
C

      IF(IDEB.EQ.2) THEN
        WRITE(LUNOUT,1079) AMSCH(NCH),AMSNE(N0),GAMF
      END IF
1079  FORMAT(/,1X,'Chi2+',2X,F9.3,5X,'Chi0',2X,F9.3,/,
     &         1X,'Widths',/,6(1X,6(3X,E9.2),/))
C
C- Branching ratios ( 3-body decays )
      GAW= GAM
      IF(ITWO.EQ.1) GAW= GAM+GA2
      DO 60 LD1=1,6
        DO 61 LD2=1,6
          GFW= GAMF(LD1,LD2)
          XBRF(LD1,LD2)= GFW/GAW
61      CONTINUE
        IF(ITWO.EQ.0) GOTO 60
        DO 62 LD2=1,6
          XBR2(LD1,LD2,1)= GA2F(LD1,LD2,1)/GAW
          XBR2(LD1,LD2,2)= GA2F(LD1,LD2,2)/GAW
62      CONTINUE
60    CONTINUE
179   FORMAT(/,1X,2I4,3(4X,F9.3))
C
C- Topological branching ratios
      XBR(1)= 0.D0
      DO 70 LD1=1,3
        XBR(1)= XBRF(LD1,LD1) + XBR(1)
        XBR(1)= XBR2(LD1,LD1,1) + XBR(1)
        XBR(1)= XBR2(LD1,LD1,2) + XBR(1)
70    CONTINUE
      XBR(2)= 0.D0
      DO 71 LD1=4,6
        DO 72 LD2=4,6
          XBR(2)= XBRF(LD1,LD2) + XBR(2)
          XBR(2)= XBR2(LD1,LD2,1) + XBR(2)
          XBR(2)= XBR2(LD1,LD2,2) + XBR(2)
72      CONTINUE
71    CONTINUE
      XBR(3)= 0.D0
C
      IF(IDEB.EQ.2) THEN
        WRITE(LUNOUT,590) GAM,XBR
590     FORMAT(/,1X,'Total width',2X,E9.2,' GeV ',/,
     &           1X,'Top br ',2(2X,F8.3),/)
      END IF
C
C----- Calculate errors
      DO 90 L1=1,NBRA
        IF(L1.EQ.1) BJJ(L1)= XBRF(1,1)
        IF(L1.EQ.1) PROB= XSUR(1,1)
        IF(L1.EQ.2) BJJ(L1)= XBRF(2,2)
        IF(L1.EQ.2) PROB= XSUR(2,2)
        IF(L1.EQ.3) BJJ(L1)= XBRF(4,4)
        IF(L1.EQ.3) PROB= XSUR(4,4)
        IF(L1.EQ.4) BJJ(L1)= XBRF(4,5)
        IF(L1.EQ.4) PROB= XSUR(4,5)
        IF(L1.EQ.5) BJJ(L1)= XBRF(3,3)
        IF(L1.EQ.5) PROB= XSUR(3,3)
        IF(L1.EQ.6) BJJ(L1)= XBRF(5,4)
        IF(L1.EQ.6) PROB= XSUR(5,4)
        IF(L1.EQ.7) BJJ(L1)= XBRF(5,5)
        IF(L1.EQ.7) PROB= XSUR(5,5)
        IF(L1.EQ.8) BJJ(L1)= XBRF(4,6)
        IF(L1.EQ.8) PROB= XSUR(4,6)
        IF(L1.EQ.9) BJJ(L1)= XBRF(5,6)
        IF(L1.EQ.9) PROB= XSUR(5,6)
        IF(L1.EQ.10) BJJ(L1)= XBRF(6,4)
        IF(L1.EQ.10) PROB= XSUR(6,4)
        IF(L1.EQ.11) BJJ(L1)= XBRF(6,5)
        IF(L1.EQ.11) PROB= XSUR(6,5)
        IF(L1.EQ.12) BJJ(L1)= XBRF(6,6)
        IF(L1.EQ.12) PROB= XSUR(6,6)
        BEJJ(L1)= 2.*(1.-PROB) + PROB*PROB
90    CONTINUE
      ALPHA= 0.D0
      DO 91 L1=1,NBRA
        ALPHA= ALPHA + BJJ(L1)*BJJ(L1)*BEJJ(L1)
91    CONTINUE
      BTOP(1)= 0.D0
      DO 92 L1=1,NBRA
        IF(L1.GT.2.AND.L1.NE.5) GOTO 92
        BETA= BJJ(L1)*BJJ(L1)*BEJJ(L1)/XBR(1)/XBR(1)
        BTOP(1)= BETA + BTOP(1)
92    CONTINUE
      BTOP(1)= DSQRT(BTOP(1)+ALPHA)
      BTOP(2)= 0.D0
      DO 93 L1=3,NBRA
        IF(L1.EQ.5) GOTO 93
        BETA= BJJ(L1)*BJJ(L1)*BEJJ(L1)/XBR(2)/XBR(2)
        BTOP(2)= BETA + BTOP(2)
93    CONTINUE
      BTOP(2)= DSQRT(BTOP(2)+ALPHA)
      ALPHA= DSQRT(ALPHA)
C
      IF(IDEB.EQ.2) THEN
        WRITE(LUNOUT,*) ' _CHI2BR_'
        WRITE(LUNOUT,501) ALPHA
        WRITE(LUNOUT,500) (K,BJJ(K),BEJJ(K),K=1,NBRA)
        WRITE(LUNOUT,500) (K,XBR(K),BTOP(K),K=1,2)
      END IF
500   FORMAT(1X,10(I4,4X,E9.3,4X,E9.3,/))
501   FORMAT(1X,' Alpha factor',4X,E9.3)
502   FORMAT(2(1X,6(3X,F9.4),/))
C
100   CONTINUE
C
C- End of the subroutine
      RETURN
      END
C*DK CHSELD
      SUBROUTINE CHSELD
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
      INTEGER LDC0(2)
      DIMENSION WLEP(2),WHAD(2)
      REAL R1
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      ITYP(1)= 1
      ITYP(2)= 1
      IF(FIRST) THEN
        LDC0(1)= LDC(1,1)
        LDC0(2)= LDC(2,1)
        WLEP(1)= 1.D0
        WLEP(2)= 1.D0
        WHAD(1)= 1.D0
        WHAD(2)= 1.D0
        IF(LDC0(1).EQ.11) THEN
          WHAD(1)= 0.D0
          WLEP(1)= 1./XBR(1)
        ELSE IF(LDC0(1).EQ.14) THEN
          WLEP(1)= 0.D0
          WHAD(1)= 1./XBR(2)
        END IF
        IF(LDC0(2).EQ.11) THEN
          WHAD(2)= 0.D0
          WLEP(2)= 1./XBR(1)
        ELSE IF(LDC0(2).EQ.14) THEN
          WLEP(2)= 0.D0
          WHAD(2)= 1./XBR(2)
        END IF
        FIRST= .FALSE.
      END IF
      IF(LDC0(1).EQ.0.AND.LDC0(2).NE.0) THEN
        R1= RNDM(R1)
        IF(R1.GT.(.5)) THEN
          IF(LDC0(2).GT.10) THEN
            WLEP0= WLEP(1)
            WHAD0= WHAD(1)
            WLEP(1)= WLEP(2)
            WHAD(1)= WHAD(2)
            WLEP(2)= WLEP0
            WHAD(2)= WHAD0
          END IF
          LDC0(1)= LDC0(2)
          LDC0(2)= 0
        END IF
      ELSE IF(LDC0(2).EQ.0.AND.LDC0(1).NE.0) THEN
        R1= RNDM(R1)
        IF(R1.GT.(.5)) THEN
          IF(LDC0(1).GT.10) THEN
            WLEP0= WLEP(2)
            WHAD0= WHAD(2)
            WLEP(2)= WLEP(1)
            WHAD(2)= WHAD(1)
            WLEP(1)= WLEP0
            WHAD(1)= WHAD0
          END IF
          LDC0(2)= LDC0(1)
          LDC0(1)= 0
        END IF
      END IF
C
      DO 5 L0=1,2
        IF(LDC0(L0).EQ.0.OR.LDC0(L0).GT.10) THEN
          R1= RNDM(R1)
          XW= 0.D0
          DO 10 L1=1,3
            XW= XW+XBRF(L1,L1)*WLEP(L0)
            IF(R1.LT.XW) THEN
              LDC(L0,1)= L1
              LDC(L0,2)= L1
              ITYP(L0)= 1
              GOTO 5
            END IF
10        CONTINUE
          DO 20 L1=4,6
            DO 21 L2=4,6
              XW= XW+XBRF(L1,L2)*WHAD(L0)
              IF(R1.LT.XW) THEN
                LDC(L0,1)= L1
                LDC(L0,2)= L2
                ITYP(L0)= 1
                GOTO 5
              END IF
21          CONTINUE
20        CONTINUE
          DO 30 L1=1,3
            XW= XW+XBR2(L1,L1,1)*WLEP(L0)
            IF(R1.LT.XW) THEN
              LDC(L0,1)= L1
              LDC(L0,2)= L1
              ITYP(L0)= 2
              GOTO 5
            END IF
            XW= XW+XBR2(L1,L1,2)*WLEP(L0)
            IF(R1.LT.XW) THEN
              LDC(L0,1)= L1
              LDC(L0,2)= L1
              ITYP(L0)= 3
              GOTO 5
            END IF
30        CONTINUE
          DO 40 L1=4,6
            DO 41 L2=4,6
              XW= XW+XBR2(L1,L2,1)*WHAD(L0)
              IF(R1.LT.XW) THEN
                LDC(L0,1)= L1
                LDC(L0,2)= L2
                ITYP(L0)= 2
                GOTO 5
              END IF
              XW= XW+XBR2(L1,L2,2)*WHAD(L0)
              IF(R1.LT.XW) THEN
                LDC(L0,1)= L1
                LDC(L0,2)= L2
                ITYP(L0)= 3
                GOTO 5
              END IF
41          CONTINUE
40        CONTINUE
          WRITE(LUNOUT,*)
     &    ' _CHSELD_ : NOT able to choose decay channel - STOP'
          CALL EXIT
        ELSE
          LD1= LDC(L0,1)
          LD2= LDC(L0,2)
          IF(LD1.LE.3) THEN
           IF(XBR2(LD1,LD1,1).GT.(0.).OR.
     &        XBR2(LD1,LD1,2).GT.(0.))    THEN
            XB= XBR2(LD1,LD1,1)+XBR2(LD1,LD1,2)
            R1= RNDM(R1)
            ITYP(L0)= 2
            IF((R1*XB).GT.XBR2(LD1,LD1,1)) ITYP(L0)= 3
           END IF
          ELSE IF(LD1.GE.4) THEN
           IF(XBR2(LD1,LD2,1).GT.(0.).OR.
     &        XBR2(LD1,LD2,2).GT.(0.))    THEN
            XB= XBR2(LD1,LD2,1)+XBR2(LD1,LD2,2)
            R1= RNDM(R1)
            ITYP(L0)= 2
            IF((R1*XB).GT.XBR2(LD1,LD2,1)) ITYP(L0)= 3
           END IF
          END IF
          IF(LD1.GE.4.AND.LD2.LT.4) THEN
            WRITE(LUNOUT,*)
     &      ' Undefined decay channel ',LDC
            CALL EXIT
          END IF
        END IF
5     CONTINUE
C
      GX= LDC(1,1)+.1
      GY= LDC(1,2)+.1
      CALL HFILL(10135,GX,GY,1.)
      GX= LDC(2,1)+.1
      GY= LDC(2,2)+.1
      CALL HFILL(10135,GX,GY,1.)
C
C
99    CONTINUE
      RETURN
      END
C*DK CNRX
      SUBROUTINE CNRX(LFD,IT3)
C-----------------------------------------------------------------
C-----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
      DIMENSION CHA(2,2)
C
      CHA(1,1)= 0.
      CHA(1,2)=-1.
      CHA(2,1)= 2./3.
      CHA(2,2)=-1./3.
C
      LFER= 1
      IF(LFD.GE.4) LFER= 2
C
C----- Loop over neutralinos
      DO L0=1,4
C------ left / right
        DO L1=1,2
          IFE= IT3 + (L1-1)*2
          IF(IFE.EQ.1) THEN
            CNFL0(L1,L0)= AMFER(LFD,IT3)*CWNE(1,L0)
            WK1= CHA(LFER,IT3)*CWNE(3,L0)
            CNFR0(L1,L0)= -ETANE(L0)*WK1 + CWNE(4,L0)
          ELSE IF(IFE.EQ.2) THEN
            CNFL0(L1,L0)= AMFER(LFD,IT3)*CWNE(2,L0)
            WK1= CHA(LFER,IT3)*CWNE(3,L0)
            CNFR0(L1,L0)= -ETANE(L0)*WK1 - CWNE(4,L0)
          ELSE IF(IFE.EQ.3) THEN
            CNFL0(L1,L0)=  CHA(LFER,IT3)*CWNE(3,L0)
            CNFR0(L1,L0)= AMFER(LFD,IT3)*CWNE(1,L0)*ETANE(L0)
          ELSE IF(IFE.EQ.4) THEN
            CNFL0(L1,L0)=  CHA(LFER,IT3)*CWNE(3,L0)
            CNFR0(L1,L0)= AMFER(LFD,IT3)*CWNE(2,L0)*ETANE(L0)
          END IF
        END DO
      END DO
C
C----- Complex conjugates
      DO L0=1,4
        DO L1=1,2
          CNFR1(L1,L0)= ETANE(L0)*CNFR0(L1,L0)
          CNFL1(L1,L0)= ETANE(L0)*CNFL0(L1,L0)
        END DO
      END DO
C
      RETURN
      END
C*DK DECARD
      SUBROUTINE DECARD
C--------------------------------------------------------------------
C! Fill common blocks with the information stored in TABL
C--------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FIXPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

C*CC FIXPAR
C*CA FIELDS
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
C*CC FIELDS
C*CA FANSFE
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
C*CC FANSFE
C*CA VARPAR
      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
C*CC VARPAR
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C*CA HIGMAS
      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC HIGMAS
C*CA BCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=50000)
      COMMON / BCS / IW(LBCS )
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
C
      XFACT= 1.D0
      NINTE= 100
      IPAW= 0
C
      IHAND= 0
      IGENE= IW(NAMIND('GENE'))
      IF(IGENE.NE.0) THEN
        ENECM= RW(IGENE+1)
        LDC(1,1)= IW(IGENE+2)
        LDC(1,2)= IW(IGENE+3)
        LDC(2,1)= IW(IGENE+4)
        LDC(2,2)= IW(IGENE+5)
        KEYRAD= IW(IGENE+6)
        XMSZ0= RW(IGENE+7)
        XGMZ0= RW(IGENE+8)
        SINW2= RW(IGENE+9)
      END IF
C
      IPARA= 0
      IGMSM= IW(NAMIND('GMSM'))
      IF(IGMSM.NE.0) THEN
        AV2V1= RW(IGMSM+1)
        AMSCM= RW(IGMSM+2)
        AMSMU= RW(IGMSM+3)
        XMSM0= RW(IGMSM+4)
        XMH10= RW(IGMSM+5)
        IPARA= 1
      END IF
C
      IGEN1= IW(NAMIND('GEN1'))
      IF(IGEN1.NE.0) THEN
        IHAND= 1
        AMSCH(1)= ABS(RW(IGEN1+ 1))/XFACT
        AMSCH(2)= ABS(RW(IGEN1+ 2))/XFACT
        AMSNE(1)= ABS(RW(IGEN1+ 3))/XFACT
        AMSNE(2)= ABS(RW(IGEN1+ 4))/XFACT
        AMSNE(3)= ABS(RW(IGEN1+ 5))/XFACT
        AMSNE(4)= ABS(RW(IGEN1+ 6))/XFACT
        AMSSN=    RW(IGEN1+ 7)/XFACT
        AMSLR=    RW(IGEN1+ 8)/XFACT
        AMSLL=    RW(IGEN1+ 9)/XFACT
        AMSUR=    RW(IGEN1+10)/XFACT
        AMSUL=    RW(IGEN1+11)/XFACT
        AMSDR=    RW(IGEN1+12)/XFACT
        AMSDL=    RW(IGEN1+13)/XFACT
        XMH10=    RW(IGEN1+14)/XFACT
        AMH20=    RW(IGEN1+15)/XFACT
        AMH30=    RW(IGEN1+16)/XFACT
        AMHCH=    RW(IGEN1+17)/XFACT
        FLDCU(1,1)= RW(IGEN1+18)
        FLDCU(1,2)= RW(IGEN1+19)
        FLDCU(2,1)= RW(IGEN1+20)
        FLDCU(2,2)= RW(IGEN1+21)
        KW= 21
        DO 10 L1=1,4
          DO 11 L2=1,4
            KW= KW+1
            FLDNE(L1,L2)= RW(IGEN1+KW)
11        CONTINUE
10      CONTINUE
        AV2V1= RW(IGEN1+38)
        IMSNE(1)= 1
        IMSNE(2)= 2
        IMSNE(3)= 3
        IMSNE(4)= 4
        FLDCV(1,1)= FLDCU(1,1)
        FLDCV(1,2)= FLDCU(1,2)
        FLDCV(2,1)= FLDCU(2,1)
        FLDCV(2,2)= FLDCU(2,2)
        ETACH(1)= RW(IGEN1+ 1)/AMSCH(1)
        ETACH(2)= RW(IGEN1+ 2)/AMSCH(2)
        ETANE(1)= RW(IGEN1+ 3)/AMSNE(1)
        ETANE(2)= RW(IGEN1+ 4)/AMSNE(2)
        ETANE(3)= RW(IGEN1+ 5)/AMSNE(3)
        ETANE(4)= RW(IGEN1+ 6)/AMSNE(4)
      END IF
C
      IF(IPARA.EQ.0.AND.IHAND.EQ.0) THEN
        WRITE(LUNOUT,*) ' _DECARD_: No parameter has been entered'
        WRITE(LUNOUT,*) ' _Action taken_: STOP'
        CALL EXIT
      END IF
C
C----- Normalize masses
      XGMSF= 3.D0
      AMSZ0= XMSZ0/XFACT
      AGMZ0= XGMZ0/XFACT
      AGMSF= XGMSF/XFACT
      AMSCM= AMSCM/XFACT*XMSZ0
      AMSMU= AMSMU/XFACT*XMSZ0
      AMSM0= XMSM0/XFACT
      AMH10= XMH10/XFACT
      ENECM= ENECM/XFACT
      AV1V2= 1./AV2V1
C----- W mass
      AMSWU= DSQRT(AMSZ0*AMSZ0*(1.D0-SINW2))
C----- Lepton Z0 production coupling
      CLPPR= .5 - 2*SINW2 + 4*SINW2**2
C
      RETURN
      END
C*DK DIACHA
      SUBROUTINE DIACHA(ISTA)
C----------------------------------------------------------------
C!Diagonalize chargino matrix
C Output  :  ISTA  status flag   =0 if O.K.
C                                =1 if discrim. < 0.0
C                                =2 if determ.  < 0.0
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C
C----- Status flag
      ISTA= 0
C-----
      DENO2= 1.+ AV1V2*AV1V2
      IF(DENO2.LT.(0.D0)) DENO2= 1.D-8
      STHEV= AV1V2/DSQRT(DENO2)
      CTHEV= 1.   /DSQRT(DENO2)
      S2THV= 2.*AV1V2/DENO2
      C2THV= (1.-AV1V2*AV1V2)/DENO2
C
      DISCR= AMSCM*AMSCM
      DISCR= AMSMU*AMSMU          + DISCR
      DISCR= 2*AMSMU*AMSCM*S2THV  + DISCR
      DISCR= 4.*(AMSWU*AMSWU)*DISCR
      DISCR= (AMSCM*AMSCM-AMSMU*AMSMU)**2 + DISCR
      AMSW2= AMSWU*AMSWU
      DISCR= 4*(AMSW2*AMSW2)*(C2THV*C2THV) + DISCR
C----- Protection
      IF(DABS(DISCR).LT.(1.D-6)) THEN
        DISCR= 1.D-8
      ELSE IF(DISCR.LT.(-1.D-6)) THEN
        ISTA= 1
        RETURN
      END IF
      DISCR= DSQRT(DISCR)
C
C----- Test different way
      TPHIM= (AMSMU*AMSMU-AMSCM*AMSCM-
     &                  2.*AMSWU*AMSWU*C2THV)-DISCR
      IF(DABS(TPHIM).LT.(1.D-6)) THEN
        CPHIM= 0.D0
        SPHIM= 1.D0
      ELSE
        TPHIM= (2.*DSQRT(2.D0)*AMSWU)/TPHIM
        TPHIM=-TPHIM*(AMSCM*STHEV+AMSMU*CTHEV)
        CPHIM= 1./DSQRT(1.+TPHIM*TPHIM)
        SPHIM= TPHIM*CPHIM
      END IF
C
      TPHIP= (AMSMU*AMSMU-AMSCM*AMSCM+
     &                  2.*AMSWU*AMSWU*C2THV)-DISCR
      IF(DABS(TPHIP).LT.(1.D-6)) THEN
        CPHIP= 0.D0
        SPHIP= 1.D0
      ELSE
        TPHIP= (2.*DSQRT(2.D0)*AMSWU)/TPHIP
        TPHIP=-TPHIP*(AMSCM*CTHEV+AMSMU*STHEV)
        CPHIP= 1./DSQRT(1.+TPHIP*TPHIP)
        SPHIP= TPHIP*CPHIP
      END IF
C
      X01TM= AMSCM*AMSCM + AMSMU*AMSMU + 2*AMSWU*AMSWU
C
C----- Heaviest chargino mass
      AMSCH(1)= (X01TM + DISCR)/2.D0
      IF(AMSCH(1).LT.(0.D0)) AMSCH(1)= 1.D-8
      AMSCH(1)= DSQRT(AMSCH(1))
C
C----- Lightest chargino mass
      AMSCH(2)= (X01TM - DISCR)/2.D0
      IF(AMSCH(2).LT.(0.D0)) AMSCH(2)= 1.D-8
      AMSCH(2)= DSQRT(AMSCH(2))
C
C----- Fields
C
      FLDCU(1,1)= CPHIM
      FLDCU(2,2)= CPHIM
      FLDCU(1,2)= SPHIM
      FLDCU(2,1)=-SPHIM
C
      FLDCV(1,1)= CPHIP
      FLDCV(2,2)=-CPHIP
      FLDCV(1,2)= SPHIP
      FLDCV(2,1)= SPHIP
C
      ETACH(1)= 1.
      ETACH(2)= 1.
C
      DETER= AMSCM*AMSMU - (AMSWU*AMSWU)*C2THV
      IF(DETER.GT.(0.D0)) THEN
        ISTA= 2
        AMSCH(1)= DABS(AMSCH(1))
        AMSCH(2)= DABS(AMSCH(2))
        ETACH(2)= -1.
      END IF
C
      RETURN
      END
C*DK DIANEU
      SUBROUTINE DIANEU(ISTA)
C----------------------------------------------------------------
C!Diagonalize neutralino matrix
C Output  :  ISTA  staus flag,   =0 if O.K.
C                                =1 if K.O.
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
      DIMENSION WYMTX(4,4),WAVCT(4),WZMTX(4,4)
      REAL RYMTX(4,4),RAVCT(4),RZMTX(4,4),RKVCT(8)
C
C----- Status flag
      ISTA= 0
C
C----- Initialize neutralino related quantities
      DO 10 L1=1,4
        AMSNE(L1)= 0.D0
        ETANE(L1)= 0.D0
        DO 11 L2=1,4
          FLDNE(L1,L2)= 0.D0
11      CONTINUE
10    CONTINUE
C-----
      DENO2= 1.+ AV1V2*AV1V2
      STHEV= AV1V2/DSQRT(DENO2)
      CTHEV= 1.   /DSQRT(DENO2)
      S2THV= 2.*AV1V2/DENO2
      C2THV= (1.-AV1V2*AV1V2)/DENO2
      SINW0= DSQRT(SINW2)
      COSW0= DSQRT(1.-SINW2)
      WMSM1= 5.*SINW2*AMSCM/(3.*(1.-SINW2))
C
C----- Fill matrix to be diagonalized
      WYMTX(1,1)= WMSM1*(1.-SINW2) + AMSCM*SINW2
      WYMTX(1,2)= (AMSCM-WMSM1)*SINW0*COSW0
      WYMTX(1,3)= 0.D0
      WYMTX(1,4)= 0.D0
      WYMTX(2,2)= AMSCM*(1.-SINW2) + WMSM1*SINW2
      WYMTX(2,3)= AMSZ0
      WYMTX(2,4)= 0.D0
      WYMTX(3,3)= AMSMU*S2THV
      WYMTX(3,4)= AMSMU*C2THV
      WYMTX(4,4)=-AMSMU*S2THV
      WYMTX(2,1)= (AMSCM-WMSM1)*SINW0*COSW0
      WYMTX(3,1)= 0.D0
      WYMTX(4,1)= 0.D0
      WYMTX(3,2)= AMSZ0
      WYMTX(4,2)= 0.D0
      WYMTX(4,3)= AMSMU*C2THV
C
      DO 20 L1=1,4
        DO 21 L2=1,4
          RYMTX(L1,L2)= WYMTX(L1,L2)
21      CONTINUE
20    CONTINUE
C
C----- Debug
      IF(IDEB.EQ.2) THEN
        WRITE(*,100) ((WYMTX(L1,L2),L2=1,4),L1=1,4)
      END IF
100   FORMAT(/,1X,'Neutral matrix before diagon.',/,
     +         4(1X,4(E9.2,4X),/))
C
C----- Call EISRS1 ( CERNLIB,F224 ) to diagonalize real
C----- symmetric matrix
      CALL EISRS1(4,4,RYMTX,RAVCT,RZMTX,LER,RKVCT)
      IF(LER.NE.0) THEN
        ISTA= 1
        RETURN
      END IF
C
C----- Store eta factors
      DO 41 J=1,4
        ETANE(J)= 1.D0
        IF(RAVCT(J).LT.(0.)) ETANE(J)= -1.D0
        RAVCT(J)= ABS(RAVCT(J))
41    CONTINUE
C
C----- Sort masses for ordering ( Cernlib, KERNLIB M101 )
      CALL SORTZV(RAVCT,IMSNE,4,1,0,0)
C
      DO 30 L1=1,4
        WAVCT(L1)= RAVCT(L1)
        DO 31 L2=1,4
          WZMTX(L1,L2)= RZMTX(L1,L2)
31      CONTINUE
30    CONTINUE
C
C----- Debug
      IF(IDEB.EQ.2) THEN
        WRITE(*,101) WAVCT
        WRITE(*,102) ((WZMTX(L1,L2),L2=1,4),L1=1,4)
      END IF
101   FORMAT(/,1X,'Neutralino mass',/,
     +         1X,4(E9.2,4X),/)
102   FORMAT(/,1X,'Fields as colums',/,
     +         4(1X,4(E9.2,4X),/))
C
C----- Store eta factors
      DO 40 J=1,4
        AMSNE(J)= RAVCT(J)
40    CONTINUE
C
C----- Store fields
      DO 50 J=1,4
        DO 51 I=1,4
          FLDNE(J,I)= WZMTX(I,J)
51      CONTINUE
50    CONTINUE
C
C
      RETURN
      END
C*DK FANAME
      SUBROUTINE FANAME
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
C*CA FASUSY
      PARAMETER( NSUSY= 17 )
      COMMON /FASUSY/ AMSUS(NSUSY),CHSUS(NSUSY),NASUS(NSUSY)
      CHARACTER*4 NASUS
C*CC FASUSY
C
C----- Names
      NASUS( 1)= 'SUPL'
      NASUS( 2)= 'SUPR'
      NASUS( 3)= 'SDWL'
      NASUS( 4)= 'SDWR'
      NASUS( 5)= 'SLEL'
      NASUS( 6)= 'SLER'
      NASUS( 7)= 'SNEU'
      NASUS( 8)= 'CH1+'
      NASUS( 9)= 'CH2+'
      NASUS(10)= 'CH00'
      NASUS(11)= 'CH01'
      NASUS(12)= 'CH02'
      NASUS(13)= 'CH03'
      NASUS(14)= 'HIG+'
      NASUS(15)= 'HG01'
      NASUS(16)= 'HG02'
      NASUS(17)= 'HG03'
C----- Masses
      AMSUS( 1)= AMSUL
      AMSUS( 2)= AMSUR
      AMSUS( 3)= AMSDL
      AMSUS( 4)= AMSDR
      AMSUS( 5)= AMSLL
      AMSUS( 6)= AMSLR
      AMSUS( 7)= AMSSN
      AMSUS( 8)= AMSCH(1)
      AMSUS( 9)= AMSCH(2)
      AMSUS(10)= AMSNE(IMSNE(1))
      AMSUS(11)= AMSNE(IMSNE(2))
      AMSUS(12)= AMSNE(IMSNE(3))
      AMSUS(13)= AMSNE(IMSNE(4))
      AMSUS(14)= AMHCH
      AMSUS(15)= AMH10
      AMSUS(16)= AMH20
      AMSUS(17)= AMH30
C----- Charges
      CHSUS( 1)= 2.D0/3.D0
      CHSUS( 2)= 2.D0/3.D0
      CHSUS( 3)=-1.D0/3.D0
      CHSUS( 4)=-1.D0/3.D0
      CHSUS( 5)=-1.D0
      CHSUS( 6)=-1.D0
      CHSUS( 7)= 0.D0
      CHSUS( 8)= 1.D0
      CHSUS( 9)= 1.D0
      CHSUS(10)= 0.D0
      CHSUS(11)= 0.D0
      CHSUS(12)= 0.D0
      CHSUS(13)= 0.D0
      CHSUS(14)= 1.D0
      CHSUS(15)= 0.D0
      CHSUS(16)= 0.D0
      CHSUS(17)= 0.D0
C
      RETURN
      END
C*DK FERENE
      SUBROUTINE FERENE(AM0,AM1,LD1,LD2,X1,X2,C12,IER)
C----------------------------------------------------------------
C! AM0 decaying particle mass
C! AM1 third particle mass
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
      PARAMETER( NRANMX= 100000 )
      REAL R1,R2
      EXTERNAL FTEST
      INTEGER  FTEST
C
      IER= 0
C
      LF=1
      IF(LD1.GE.4) LF=2
C
      NRAN= 0
1     CONTINUE
      NRAN= NRAN+1
      IF(NRAN.GT.NRANMX) THEN
        IER= 1
        RETURN
      END IF
      R1= RNDM(R1)
      X1= ALIM*R1
      AM20= ALIM-X1
      AM21= (AM0-AM1*AM1/(AM0-2.*X1))/2.D0
      X1= X1/AM0
      R2= RNDM(R2)
      X2= AM20 + (AM21-AM20)*R2
      X2= X2/AM0
      J= FTEST(X1,X2,C12)
      IF(J.EQ.0) GOTO 1
      X1= X1*AM0
      X2= X2*AM0
      GX= SWADIF(X1,X2,LF)
      R2= RNDM(R1)
      IF(R2.GT.GX/WEIMAX(LD1,LD2)) GOTO 1
C
      RETURN
      END
C*DK FERMMS
      SUBROUTINE FERMMS
C----------------------------------------------------------------
C!FERMions MaSses
C
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C
      AMFER(1,1)= 0.D0
      AMFER(1,2)= .511D-3/XFACT
      AMFER(2,1)= 0.D0
      AMFER(2,2)= .1056  /XFACT
      AMFER(3,1)= 0.D0
      AMFER(3,2)= 1.784  /XFACT
      AMFER(4,1)= .325   /XFACT
      AMFER(4,2)= .325   /XFACT
      AMFER(5,1)= 1.5    /XFACT
      AMFER(5,2)= .5     /XFACT
      AMFER(6,1)= 80.    /XFACT
      AMFER(6,2)= 5.     /XFACT
C
      RETURN
      END
C*DK FILCOS
      SUBROUTINE FILCOS(N1,N0,IFER)
C-----------------------------------------------------------------
C! Fill constants
C-----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C
C- Define constants
      BS= AMDEC(1)*AMDEC(1)
      CS= AMDEC(1)*AMDEC(1)-AMDEC(4)*AMDEC(4)
      DS= 2*AMDEC(1)
      ES= AMDEC(2)*AMDEC(3)
      FS= AMDEC(1)*AMDEC(4)
      GS= AMDEC(4)*AMDEC(4)
C
      CL= CFELF(IFER)
      CR= CFERG(IFER)
      OL= CO2LF(N0,N1)
      OR= CO2RG(N0,N1)
      W1= (AMDEC(2)+AMDEC(3))*(AMDEC(1)-AMDEC(4))
      W2= AMDEC(3)*AMDEC(1)
C
      RETURN
      END
C*DK FILINV
      SUBROUTINE FILINV(X1,X2)
C-----------------------------------------------------------------
C-----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C
C
      XINV(1,2)= DS/2.D0*X1
      XINV(1,3)= DS/2.D0*X2
      XINV(1,4)= DS/2.D0*(DS/2.D0-X1-X2)
      XINV(2,3)=-(CS+2.*ES-DS*(X1+X2))/2.D0
      XINV(2,4)= (CS-DS*X2)/2.D0
      XINV(3,4)= (CS-DS*X1)/2.D0
C
      XINV(2,1)= XINV(1,2)
      XINV(3,1)= XINV(1,3)
      XINV(4,1)= XINV(1,4)
      XINV(3,2)= XINV(2,3)
      XINV(4,2)= XINV(2,4)
      XINV(4,3)= XINV(3,4)
C
      WM0= BS+GS-DS*(DS/2.D0-X1-X2)
      WM1= BS+ES-DS*X1
      WM2= BS+ES-DS*X2
C
      RETURN
      END
C*DK FSUR
      DOUBLE PRECISION FUNCTION FSUR(AM0,AM3)
C-----------------------------------------------------------------
C-----------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C
      FSUR= AM0*AM0/8.D0
      IF((AM3/AM0).LT.(1.D-8)) RETURN
C
      WK1= AM3/AM0
      WK2= 1.D0-WK1*WK1
      WK3= 2.*DLOG(WK1)+WK2
      FSUR= FSUR*WK2*WK2 + AM3*AM3*WK3/4.
C
      RETURN
      END
C*DK FTEST
      INTEGER FUNCTION FTEST(X1,X2,C12)
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /MASSES/ XM1,XM2,XM3
C
      FTEST= 0
      IF((X1+X2).GT.(1.-XM3)) RETURN
      IF(X1.LT.XM1) RETURN
      IF(X2.LT.XM2) RETURN
C
      GTEST= 1.+ XM1*XM1 + XM2*XM2 - XM3*XM3
      GTEST= GTEST - 2.*(X1+X2) + 2.*X1*X2
      G1= X1*X1-XM1*XM1
      G2= X2*X2-XM2*XM2
      IF(G1.LE.(1.D-10).OR.G2.LE.(1.D-10)) RETURN
      GDENO= 2.*DSQRT(G1)*DSQRT(G2)
      C12= GTEST/GDENO
      GTEST= DABS(C12)
      IF(GTEST.LT.(1.)) FTEST= 1
C
      RETURN
      END
C*DK GETIFL
      SUBROUTINE GETIFL(ICH,I1,I2,IFL1,IFL2)
C----------------------------------------------------------------
C----------------------------------------------------------------
C
      IF(I1.EQ.4) THEN
        IF(I2.EQ.4) THEN
          IFL1= 17
          IFL2= 23
        ELSE IF(I2.EQ.5) THEN
          R1= RNDM(R1)
          IF(R1.LT.(.5)) THEN
            IFL1= 18
            IFL2= 23
          ELSE
            IFL1= 17
            R2= RNDM(R2)
            IFL2= 37
            IF(R2.GT.(.5)) IFL2= 38
          END IF
        ELSE IF(I2.EQ.6) THEN
          R1= RNDM(R1)
          IF(R1.LT.(.5)) THEN
            IFL1=-101
            IFL2= 23
          ELSE
            IFL1= 17
            IFL2= 102
          END IF
        END IF
      ELSE IF(I1.EQ.5) THEN
        IF(I2.EQ.4) THEN
          R1= RNDM(R1)
          IF(R1.LT.(.5)) THEN
            IFL1= 17
            IFL2= 20
          ELSE
            IFL1= 21
            IFL2= 23
          END IF
        ELSE IF(I2.EQ.5) THEN
          R1= RNDM(R1)
          IF(R1.LT.(.5)) THEN
            IFL1= 18
            IFL2= 20
          ELSE
            IFL1= 21
            R2= RNDM(R2)
            IFL2= 37
            IF(R2.GT.(.5)) IFL2= 38
          END IF
        ELSE IF(I2.EQ.6) THEN
          R1= RNDM(R1)
          IF(R1.LT.(.5)) THEN
            IFL1=-101
            IFL2= 20
          ELSE
            IFL1= 21
            IFL2= 102
          END IF
        END IF
      ELSE IF(I1.EQ.6) THEN
        IF(I2.EQ.4) THEN
          R1= RNDM(R1)
          IF(R1.LT.(.5)) THEN
            IFL1= 17
            IFL2= 105
          ELSE
            IFL1= 106
            IFL2= 23
          END IF
        ELSE IF(I2.EQ.5) THEN
          R1= RNDM(R1)
          IF(R1.LT.(.5)) THEN
            IFL1= 18
            IFL2= 105
          ELSE
            IFL1= 106
            R2= RNDM(R2)
            IFL2= 37
            IF(R2.GT.(.5)) IFL2= 38
          END IF
        ELSE IF(I2.EQ.6) THEN
          R1= RNDM(R1)
          IF(R1.LT.(.5)) THEN
            IFL1=-101
            IFL2= 105
          ELSE
            IFL1= 106
            IFL2= 102
          END IF
        END IF
      END IF
C
      IF(ICH.EQ.2) THEN
C------- Charge conjugation
        IF(IFL1.NE.23.AND.IFL1.NE.37.AND.IFL1.NE.38)
     &                                           IFL1=-IFL1
        IF(IFL2.NE.23.AND.IFL2.NE.37.AND.IFL2.NE.38)
     &                                           IFL2=-IFL2
      END IF
C
      RETURN
      END
C*DK GETMOM
      SUBROUTINE GETMOM(QIN0,CTH12,QW1,QW2,QW3,IER)
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C*CA MASSES
      COMMON /MASSES/ XM1,XM2,XM3
C*CC MASSES
      DIMENSION QIN0(5)
      DIMENSION QW1(5),QW2(5),QW3(5),QCHI(5)
      REAL R1,R2
C
      IER= 0
C
      WOK3= QW1(4)*QW1(4)-QW1(5)*QW1(5)
      WOK4= QW2(4)*QW2(4)-QW2(5)*QW2(5)
      IF(WOK3.GT.(1.D-6).AND.WOK4.GT.(1.D-6)) THEN
C----- Angle between 1 and 2
        STH12= 1.-CTH12*CTH12
        IF(STH12.LE.(0.D0)) THEN
          WRITE(LUNOUT,*) CTH12
          IER= -1
          RETURN
        END IF
        STH12= DSQRT(STH12)
C----- Momenta
        QW1(1)= 0.D0
        QW1(2)= DSQRT(WOK3)
        QW1(3)= 0.D0
        QW2(1)= DSQRT(WOK4)*STH12
        QW2(2)= DSQRT(WOK4)*CTH12
        QW2(3)= 0.D0
        QW3(1)=-QW2(1)
        QW3(2)=-QW1(2)-QW2(2)
        QW3(3)= 0.D0
      ELSE
        WRITE(LUNOUT,179) QIN0,QW1,QW2,QW3
        IER= 1
        RETURN
      END IF
179   FORMAT(//,4(1X,5(3X,F9.3),/))
C
      RETURN
      END
C*DK IFLVOU
      INTEGER FUNCTION IFLVOU(I1,I2)
C----------------------------------------------------------------
C! Get LUND code for fermion I1,I2
C----------------------------------------------------------------
C
      IFLVOU= 0
C
      IF(I1.LE.3) THEN
        IFLVOU= I2 + 6 + (I1-1)*2
      ELSE IF(I1.EQ.4) THEN
        IFLVOU= 500 + I2
      ELSE IF(I1.GE.5) THEN
        IFLVOU= 503 + (I1-4)*2 - I2
      END IF
C
      RETURN
      END
C*DK INIRAD
      SUBROUTINE INIRAD(SIG,IER)
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
      EXTERNAL CHASIG
C
        IER= 0
      IF(KEYRAD.EQ.0) THEN
        SS= ENECM*ENECM
        XS= CHASIG(SS)
        WRITE(LUNOUT,100) XS
      ELSE
        EBEAM= ENECM/2.
        STHR= 4.D0*AMSCH(2)*AMSCH(2)
        INDEX= 1
        IPRI = 1
        CALL REMT1(EBEAM,CHASIG,STHR,INDEX,SIG1,IPRI)
        SIG= SIG1
        IF(IPRI.EQ.-1) IER= -1
      END IF
100   FORMAT(/,1X,' Nonradiative cross section',D15.6,' nb',//)
C
C- Get YMAX
      NSTX0= 1000
      CMAX0= 1.
      ICHA0= 2
      ITER0= 4
      XS= XSINTE(NSTX0,ENECM,CMAX0,ICHA0,ICHA0,ITER0,YMX)
      YMAX= YMX
C
      RETURN
      END
C*DK INITIA
      SUBROUTINE INITIA(IER)
C----------------------------------------------------------------
C! Initialize the job
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C
      IER= 0
C
C----- integer delta function
      IDELT(1,1)= 1
      IDELT(1,2)= 0
      IDELT(2,1)= 0
      IDELT(2,2)= 1
C
      IF(IHAND.EQ.0) THEN
C- Diagonalize Chargino matrix
        CALL DIACHA(ISTCH)
C
C- Diagonalize Neutralino matrix
        CALL DIANEU(ISTNE)
        IF(ISTNE.EQ.1) THEN
          IER= 2
          GOTO 999
        END IF
      END IF
C
C- Calculate couplings
      CALL CALCOU
      IF(IHAND.EQ.0) THEN
C- Calculate scalar-fermion masses
        CALL SCFMMS
C- Calculate MH10 mass
        CALL MH1MIN(IER)
        IF(IER.NE.0) THEN
          IER= 3
          GOTO 999
        END IF
C- Calculate Higgs masses
        CALL CALHMS(IER)
        IF(IER.NE.0) THEN
          IER= 3
          GOTO 999
        END IF
      END IF
C- Fill fermion masses
      CALL FERMMS
C----- Define masses ( fill /FANAME/ )
      CALL FANAME
C
999   CONTINUE
C
      RETURN
      END
C*DK LORAX
      SUBROUTINE LORAX(IAX,PAX,AMS,QVC)
C----------------------------------------------------------------
C! PAX= momentum, AMS= mass
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
      DIMENSION QVC(5),WVC(4)
C
      DO 5 L1=1,4
        WVC(L1)= QVC(L1)
5     CONTINUE
      AEN= DSQRT(PAX*PAX+AMS*AMS)
      BET= PAX/AEN
      GAM= AEN/AMS
      IF(IAX.EQ.1) THEN
        QVC(1)= GAM*(WVC(1)+BET*WVC(4))
        QVC(2)= WVC(2)
        QVC(3)= WVC(3)
        QVC(4)= GAM*(WVC(4)+BET*WVC(1))
      ELSE IF(IAX.EQ.2) THEN
        QVC(1)= WVC(1)
        QVC(2)= GAM*(WVC(2)+BET*WVC(4))
        QVC(3)= WVC(3)
        QVC(4)= GAM*(WVC(4)+BET*WVC(2))
      ELSE IF(IAX.EQ.3) THEN
        QVC(1)= WVC(1)
        QVC(2)= WVC(2)
        QVC(3)= GAM*(WVC(3)+BET*WVC(4))
        QVC(4)= GAM*(WVC(4)+BET*WVC(3))
      END IF
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
C*DK MH1MIN
      SUBROUTINE MH1MIN(IER)
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
      DIMENSION AQW1(4),AQW2(4),AQM1(4),AV210(4)
      DATA AQW1 / 40., 39., 33., 39. /
      DATA AQW2 /  0., .35, .46, .76 /
      DATA AQM1 / -2.86, -54.6, 20., 5.25 /
      DATA AV210 / 1., 2.25, 2.9, 5.7 /
C
      IER= 0
C
      IF(AMH10.GT.0.) RETURN
      AV2V1= 1./AV1V2
      DO 10 L1=1,4
        IF(AV2V1.GT.AV210(L1)) THEN
          AMH10= AQW1(L1)+AQM1(L1)*(DLOG10(AV2V1)-AQW2(L1))
        END IF
10    CONTINUE
      IF(AMH10.LT.(0.0)) THEN
        IER= 1
        RETURN
      ELSE
        AMH10= AMH10/XFACT
      END IF
C
      RETURN
      END
C*DK PHOTOT
      DOUBLE PRECISION FUNCTION PHOTOT(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Production term due to photon exchange
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*IF DOC
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
      IDTIJ= IDELT(ICHAR,JCHAR)
C
      PHOTOT= AECHI*AECHJ+APCHI*APCHI/3.+AMASI*AMASJ
      PHOTOT= APCHI*DSQRT(ASVAR)*IDTIJ*PHOTOT
      PHOTOT= PHOTOT/ASVAR/ASVAR/ASVAR
      PHOTOT= (AEPAR**2)/(2.*APIGR)*PHOTOT
C
      RETURN
      END
C*DK PHOTPD
      DOUBLE PRECISION FUNCTION PHOTPD(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Production term due to photon exchange
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
      IDTIJ= IDELT(ICHAR,JCHAR)
C
      PHOTPD= (AMASI**2-AUVAR)*(AMASJ**2-AUVAR)
      PHOTPD= (AMASI**2-ATVAR)*(AMASJ**2-ATVAR) + PHOTPD
      PHOTPD= 2.*AMASI*AMASJ*ASVAR              + PHOTPD
      PHOTPD= (AEPAR**2)*IDTIJ*PHOTPD
      PHOTPD= PHOTPD/(8.*APIGR*(ASVAR**4))
C
      RETURN
      END
C*DK PHSNPD
      DOUBLE PRECISION FUNCTION PHSNPD(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Inteference term : photon-scalar neutrino
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
C
      WCS00= (FLDCV(ICHAR,1)**2)*IDELT(ICHAR,JCHAR)
C
      PHSNPD= (AMASI**2-ATVAR)*(AMASJ**2-ATVAR)
      PHSNPD= AMASI*AMASJ*ASVAR + PHSNPD
      PHSNPD= PHSNPD * WCS00
      PHSNPD= (AEPAR**2)*ADSNU*PHSNPD/SINW2
      PHSNPD= PHSNPD/(16.*APIGR*(ASVAR**3))
C
      RETURN
      END
C*DK PHZ0PD
      DOUBLE PRECISION FUNCTION PHZ0PD(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Inteference term : photon-Z0
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
      IDTIJ= IDELT(ICHAR,JCHAR)
C
      WCS01= CO1LF(ICHAR,JCHAR) + CO1RG(ICHAR,JCHAR)
      WCS01= (CFELF(2) + CFERG(2))/2. * WCS01
      WCS02= CO1LF(ICHAR,JCHAR) - CO1RG(ICHAR,JCHAR)
      WCS02= (CFELF(2) - CFERG(2))/2. * WCS02
C
      PHZ0P1= (AMASI**2-AUVAR)*(AMASJ**2-AUVAR)
      PHZ0P1= (AMASI**2-ATVAR)*(AMASJ**2-ATVAR) + PHZ0P1
      PHZ0P1= 2.*AMASI*AMASJ*ASVAR              + PHZ0P1
      PHZ0P1= PHZ0P1 * WCS01
C
      PHZ0P2= (AMASI**2-AUVAR)*(AMASJ**2-AUVAR)
      PHZ0P2=-(AMASI**2-ATVAR)*(AMASJ**2-ATVAR) + PHZ0P2
      PHZ0P2=-PHZ0P2 * WCS02
C
      PHZ0PD= PHZ0P1 + PHZ0P2
      PHZ0PD= PHZ0PD * IDTIJ
      PHZ0PD= PHZ0PD*(AEPAR**2)*AREDZ/(SINW2*(1.-SINW2))
      PHZ0PD= PHZ0PD/(16.*APIGR*(ASVAR**3))
C
      RETURN
      END
C*DK PHZTOT
      DOUBLE PRECISION FUNCTION PHZTOT(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Inteference term : photon-Z0
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
      IDTIJ= IDELT(ICHAR,JCHAR)
C
      WCS00= AEPAR**2 / SINW2 / (1-SINW2)
      WCS00= WCS00/4/APIGR * APCHI * DSQRT(ASVAR) / ASVAR**2
      WCS00= WCS00 * AREDZ * IDTIJ
C
      WCS01= CO1LF(ICHAR,JCHAR) + CO1RG(ICHAR,JCHAR)
      WCS01= (CFELF(2) + CFERG(2))/2. * WCS01
C
      PHZTOT= AECHI*AECHJ+APCHI*APCHI/3+AMASI*AMASJ
      PHZTOT= WCS00*WCS01*PHZTOT
C
      RETURN
      END
C*DK PRCCBR
      SUBROUTINE PRCCBR(ICC)
C----------------------------------------------------------------
C! Print Chi+(ICC) branching ratios
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
C*CA FASUSY
      PARAMETER( NSUSY= 17 )
      COMMON /FASUSY/ AMSUS(NSUSY),CHSUS(NSUSY),NASUS(NSUSY)
      CHARACTER*4 NASUS
C*CC FASUSY
C*CA CCH0DC
      PARAMETER( NDCMX= 100 )
      COMMON /CCH0DC/ ICH0(NDCMX),GCH0(NDCMX),DCH0(NDCMX),
     &                BCH0(NDCMX),ACH0(NDCMX)
      CHARACTER*16 ACH0
C*CC CCH0DC
      CHARACTER*4 AFER(6,2)
      DATA AFER/'NUEL','NUMU','NUTA','UP  ',
     &          'CHRM','TOP ','ELEC','MUON',
     &          'TAU ','DOWN','STRG','BOTM'/
C
      DO 5 L1=1,NDCMX
        ICH0(L1)= 0
        GCH0(L1)= 0.D0
        DCH0(L1)= 0.D0
        BCH0(L1)= 0.D0
5     CONTINUE
C
      XSTA= DSQRT(DFLOAT(NINTE))
C
      ITWO= 0
      LCC= 7+ICC
      DO 6 L1=1,7
        IF(AMSUS(LCC).GT.AMSUS(L1)) ITWO= 1
6     CONTINUE
C
      NDEC= 0
      INCA= 0
      IOP= 1
      IF(ITWO.EQ.1) THEN
        CALL CHI2BR(IOP,ICC,1,IER)
        IF(IOP.EQ.-1) THEN
          WRITE(LUNOUT,*)
     &    ' _PRCCBR_ :  Time limit reached in CHI2BR'
          CALL EXIT
        END IF
        IF(IER.NE.0) GOTO 4
C------- Two-bodies decays ( sf-fbar & f-sfbar )
        DO 10 L0=1,2
          DO 11 L1=1,6
            DO 12 L2=1,6
              IF(GA2F(L1,L2,L0).GT.(0.D0)) THEN
                NDEC= NDEC+1
                GCH0(NDEC)= GA2F(L1,L2,L0)
                BCH0(NDEC)= XBR2(L1,L2,L0)*100.D0
                ICH0(NDEC)= 1
                IF(L0.EQ.1) THEN
                  ISTA= 7
                  IF(L1.GE.4) ISTA= 1
                ELSE
                  ISTA= 5
                  IF(L1.GE.4) ISTA= 3
                END IF
                IFE= 2
                IF(L0.EQ.2) IFE= 1
                ACH0(NDEC)= AFER(L1,IFE)//'  '//NASUS(ISTA)
              END IF
12          CONTINUE
11        CONTINUE
10      CONTINUE
      END IF
4     CONTINUE
C----- Loop over neutralino  states
      DO 9 N0=1,4
        NDC0= 0
        INC0= 0
        IC0= IMSNE(N0)
        LN0= 9+N0
C------ First look at two body decays
C------ Decay into charged higgs and neutralino
        IF(AMSUS(LCC).GT.(AMSUS(LN0)+AMSUS(14))) THEN
          INCA= INCA+1
          INC0= INC0+1
          NDEC= NDEC+1
          ICH0(NDEC)= 2
          ACH0(NDEC)= NASUS(LN0)//' '//NASUS(14)
        END IF
C------ Decay into W+ and neutralino
        IF(AMSUS(LCC).GT.(AMSUS(LN0)+AMSWU)) THEN
          INCA= INCA+1
          INC0= INC0+1
          NDEC= NDEC+1
          ICH0(NDEC)= 2
          ACH0(NDEC)= NASUS(LN0)//' W+'
        END IF
        IF(INC0.GT.0.AND.ITWO.EQ.0) GOTO 9
C
        CALL CHI2BR(IOP,ICC,IC0,IER)
        IF(IOP.EQ.-1) THEN
          WRITE(LUNOUT,*)
     &    ' _PRCCBR_ :  Time limit reached in CHI2BR'
          CALL EXIT
        END IF
        IF(IER.NE.0) GOTO 9
        GLEP= 0.D0
        GHAD= 0.D0
        IF(INC0.EQ.0) THEN
C--------- Three-body decays
          DO 20 L1=1,6
            DO 21 L2=1,6
              IF(GAMF(L1,L2).GT.(0.D0)) THEN
                NDEC= NDEC+1
                NDC0= NDC0+1
                IF(L1.LE.3) GLEP= GLEP + GAMF(L1,L2)
                IF(L1.GE.4) GHAD= GHAD + GAMF(L1,L2)
                GCH0(NDEC)= GAMF(L1,L2)
                BCH0(NDEC)= XBRF(L1,L2)*100.D0
                ICH0(NDEC)= 1
                ACH0(NDEC)=
     &          NASUS(LN0)//' '//AFER(L1,1)//' '//AFER(L2,2)
              END IF
21          CONTINUE
20        CONTINUE
        END IF
C------ Topological decays
        IF(NDC0.GT.0) THEN
          NDEC= NDEC+1
          GCH0(NDEC)= GLEP
          BCH0(NDEC)= XBR(1)*100.D0
          ICH0(NDEC)= 10
          ACH0(NDEC)= NASUS(LN0)//' LEPTONS'
          NDEC= NDEC+1
          GCH0(NDEC)= GHAD
          BCH0(NDEC)= XBR(2)*100.D0
          ICH0(NDEC)= 10
          ACH0(NDEC)= NASUS(LN0)//' HADRONS'
        END IF
9     CONTINUE
C--- Radiative decay and decays into neutral higgses
      IF(ICC.EQ.1) THEN
        NDEC= NDEC+1
        INCA= INCA+1
        ICH0(NDEC)= 2
        ACH0(NDEC)= NASUS(9)//' GAMMA'
        DO 30 L1=1,3
          LH0= 14+L1
          IF(AMSUS(8).LE.(AMSUS(9)+AMSUS(LH0))) GOTO 30
          NDEC= NDEC+1
          INCA= INCA+1
          ICH0(NDEC)= 2
          ACH0(NDEC)= NASUS(9)//' '//NASUS(LH0)
30      CONTINUE
      END IF
C
C- Renormalize branching ratios
      GTOT= 0.D0
      DO 50 L1=1,NDEC
        IF(ICH0(L1).EQ.1) GTOT= GTOT+GCH0(L1)
50    CONTINUE
      DO 51 L1=1,NDEC
        IF(ICH0(L1).NE.2) BCH0(L1)= GCH0(L1)/GTOT*100.D0
51    CONTINUE
C
      LCC= 7+ICC
      IF(NDEC.EQ.0) THEN
        WRITE(LUNOUT,899) NASUS(LCC)
        GOTO 999
      ELSE IF(NDEC.EQ.INCA) THEN
        WRITE(LUNOUT,900) NASUS(LCC)
        GOTO 120
      END IF
      WRITE(LUNOUT,900) NASUS(LCC)
      WRITE(LUNOUT,905)
      DO 105 L1=1,NDEC
        IF(ICH0(L1).EQ.2) GOTO 105
        WRITE(LUNOUT,901) ACH0(L1),GCH0(L1),BCH0(L1)
105   CONTINUE
120   CONTINUE
      IF(INCA.EQ.0) THEN
        WRITE(LUNOUT,898)
        GOTO 999
      END IF
      IF(NDEC.EQ.INCA) THEN
        WRITE(LUNOUT,906)
      ELSE
        WRITE(LUNOUT,902)
      END IF
      DO 110 L1=1,NDEC
        IF(ICH0(L1).NE.2) GOTO 110
        WRITE(LUNOUT,903) ACH0(L1)
110   CONTINUE
      IF(NDEC.EQ.INCA) WRITE(LUNOUT,897)
897   FORMAT(/,1X,'No decay width has been calculated',//)
898   FORMAT(/,1X,'All allowed decays have been calculated',//)
899   FORMAT(/,1X,A16,' is stable',//)
900   FORMAT(/,1X,A16,' branching ratios',/)
901   FORMAT(1X,A16,3X,E9.3,3X,3X,F9.3)
902   FORMAT(/,1X,'Decays allowed but not calculated',/)
903   FORMAT(1X,A16)
905   FORMAT(/,1X,'Channel',9X,'Width(GeV)',6X,
     &         6X,'BR(%)',11X,/)
906   FORMAT(/,1X,'Decays allowed',/)
C
999   CONTINUE
      RETURN
      END
C*DK PSNTOT
      DOUBLE PRECISION FUNCTION PSNTOT(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Inteference term : photon-scalar neutrino
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
C
      WCS00=-(FLDCV(ICHAR,1)**2)*IDELT(ICHAR,JCHAR)
      WCS00= AEPAR**2 * WCS00 / SINW2 / (16*APIGR)
      WCS00= WCS00 / ASVAR**2
C
      PSNTOT= DLOG(DABS((AAVAR+ABVAR)/(AAVAR-ABVAR)))
      PSNTOT= AMASI*AMASJ*PSNTOT + AHVAR
      PSNTOT= PSNTOT * WCS00
C
      RETURN
      END
C*DK REMT1
      SUBROUTINE REMT1(EBEAM,CROSS,STHR,INDEX,SIG1,IPRI)
C-----------------------------------------------------------------------
C!This part initializes the initial-state radiator.
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
      DIMENSION MINDEX(12)
      DATA MINDEX/2,1,3,1,4,1,5,6,7,8,9,10/
C
C DEFINITION OF BREMSSTRAHLUNG SPECTRUM
      SPECTR(XK)=FAC*(1.+(1.-XK)**2)/XK*CROSS(S*(1.-XK))
      INIT= 0
C        Store EBEAM into local variable EBEA for later use
      EBEA = EBEAM
C INITIALIZE A FEW QUANTITIES AND CONSTANTS
      S  = 4.*EBEAM**2
C CHECK IF PRODUCTION IS POSSIBLE
      IF(S.LE.STHR) THEN
      IPRI= -1
      RETURN
      END IF
      XPS= (.511D-03/EBEAM)**2/2.
      XPT= (2.+XPS)/XPS
      XPL= DLOG(XPT)
      PI = 4.*DATAN(1.D0)
      TPI= 2.*PI
      ALF= 1./137.036D0
      FAC= ALF/PI*(XPL-1.)
      XKC= DEXP( - ( PI/(2.*ALF) + 3./4.*XPL + PI**2/6. - 1. )
     .            /( XPL - 1. )                           )
      IF(IPRI.EQ.0) GO TO 800
      INIT = 1
      PRINT 1,EBEAM,XKC
    1 FORMAT(
     .  2X,'Initialisation of routine REMT :',/,
     .  3X,'Beam energy  : ',F7.3,' GEV',/,
     .  3X,'Minimal bremsstrahlung energy : ',D15.9,' * Ebeam')
  800 CONTINUE
C
C PARAMETERS OF NUMERICAL INTEGRATION STEP
      N    = 100
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
      IF(IPRI.EQ.0) RETURN
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
C*DK ROTAX
      SUBROUTINE ROTAX(IAX,PHI,XVC)
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
      DIMENSION XVC(5),WVC(3)
C
      DO 5 L1=1,3
        WVC(L1)= XVC(L1)
5     CONTINUE
      IF(IAX.EQ.1) THEN
        XVC(1)= WVC(1)
        XVC(2)= WVC(2)*DCOS(PHI)-WVC(3)*DSIN(PHI)
        XVC(3)= WVC(2)*DSIN(PHI)+WVC(3)*DCOS(PHI)
      ELSE IF(IAX.EQ.2) THEN
        XVC(1)= WVC(3)*DCOS(PHI)-WVC(1)*DSIN(PHI)
        XVC(2)= WVC(2)
        XVC(3)= WVC(3)*DSIN(PHI)+WVC(1)*DCOS(PHI)
      ELSE IF(IAX.EQ.3) THEN
        XVC(1)= WVC(1)*DCOS(PHI)-WVC(2)*DSIN(PHI)
        XVC(2)= WVC(1)*DSIN(PHI)+WVC(2)*DCOS(PHI)
        XVC(3)= WVC(3)
      END IF
C
      RETURN
      END
C*DK SCFMMS
      SUBROUTINE SCFMMS
C----------------------------------------------------------------
C!SCalar FerMions MaSses
C
C If IDEB= 2  print masses and relevant parameters
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C
C----- Lower limits
      AMSL0= 43.D0*43.D0
      AMSN0= 37.D0*37.D0
      AMSQ0= 50.D0*50.D0
C
C----- Different notation
      AV2V1= 1./AV1V2
C
      C2THV= (AV2V1*AV2V1-1)/(AV2V1*AV2V1+1)
C
      AMSG = 8.*AMSCM*SINW2/3.
      AMSG2= AMSG*AMSG
      AMZ2 = AMSZ0*AMSZ0
C
      IF(AMSM0.GT.(0.)) THEN
        AM02 = AMSM0*AMSM0
C
        AMSDL= AM02 + 0.43*AMZ2*C2THV + 30.2*AMSG2
        AMSDR= AM02 + 0.07*AMZ2*C2THV + 28.4*AMSG2
        AMSUL= AM02 - 0.36*AMZ2*C2THV + 30.2*AMSG2
        AMSUR= AM02 - 0.14*AMZ2*C2THV + 28.4*AMSG2
        AMSLL= AM02 + 0.28*AMZ2*C2THV +  2.2*AMSG2
        AMSLR= AM02 + 0.22*AMZ2*C2THV +  0.6*AMSG2
        AMSSN= AM02 - 0.50*AMZ2*C2THV +  2.2*AMSG2
C
      ELSE
        AMSLL= AMSL0
        AM02= AMSL0 - 0.28*AMZ2*C2THV -  2.2*AMSG2
        IF(AM02.LT.(.0)) THEN
          AM02= 0.D0
          AMSLL= AM02 + 0.28*AMZ2*C2THV +  2.2*AMSG2
        END IF
        AMSSN= AM02 - 0.50*AMZ2*C2THV +  2.2*AMSG2
        IF(AMSSN.LT.AMSN0) THEN
          AM02= AMSN0 + 0.50*AMZ2*C2THV -  2.2*AMSG2
          AMSSN= AMSN0
          IF(AM02.LT.(.0)) THEN
            AM02= 0.D0
            AMSSN= AM02 - 0.50*AMZ2*C2THV +  2.2*AMSG2
          END IF
          AMSLL= AM02 + 0.28*AMZ2*C2THV +  2.2*AMSG2
        END IF
C
        AMSDL= AM02 + 0.43*AMZ2*C2THV + 30.2*AMSG2
        AMSDR= AM02 + 0.07*AMZ2*C2THV + 28.4*AMSG2
        AMSUL= AM02 - 0.36*AMZ2*C2THV + 30.2*AMSG2
        AMSUR= AM02 - 0.14*AMZ2*C2THV + 28.4*AMSG2
        AMSLR= AM02 + 0.22*AMZ2*C2THV +  0.6*AMSG2
C
      END IF
C
      AMMIN= 1.E8
      IF(AMSDL.LT.(0.D0)) AMSDL= AMSQ0
      AMSDL= DSQRT(AMSDL)
      IF(AMSDL.LT.AMMIN) AMMIN= AMSDL
C
      IF(AMSDR.LT.(0.D0)) AMSDR= AMSQ0
      AMSDR= DSQRT(AMSDR)
      IF(AMSDR.LT.AMMIN) AMMIN= AMSDR
C
      IF(AMSUL.LT.(0.D0)) AMSUL= AMSQ0
      AMSUL= DSQRT(AMSUL)
      IF(AMSUL.LT.AMMIN) AMMIN= AMSUL
C
      IF(AMSUR.LT.(0.D0)) AMSUR= AMSQ0
      AMSUR= DSQRT(AMSUR)
      IF(AMSUR.LT.AMMIN) AMMIN= AMSUR
C
      IF(AMSLL.LT.(0.D0)) AMSLL= AMSL0
      AMSLL= DSQRT(AMSLL)
      IF(AMSLL.LT.AMMIN) AMMIN= AMSLL
C
      IF(AMSLR.LT.(0.D0)) AMSLR= AMSL0
      AMSLR= DSQRT(AMSLR)
      IF(AMSLR.LT.AMMIN) AMMIN= AMSLR
C
      IF(AMSSN.LT.(0.D0)) AMSSN= AMSN0
      AMSSN= DSQRT(AMSSN)
      IF(AMSSN.LT.AMMIN) AMMIN= AMSSN
C
      AMSM0= DSQRT(AM02)
      ADIFF= AMSNE(IMSNE(2))-AMSNE(IMSNE(1))
      IF(IDEB.EQ.2) THEN
        WRITE(LUNOUT,8) AMSM0,AV1V2,AMSCM,AMSUR,AMSUL,AMSDR,AMSDL,
     +           AMSLR,AMSLL,AMSSN,ADIFF
      END IF
8     FORMAT(/,1X,3(4X,F9.2),/,1X,7(4X,F9.2),/,5X,F9.2,/)
      RETURN
C
      END
C*DK SNETOT
      DOUBLE PRECISION FUNCTION SNETOT(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Production term due to scalar neutrino exchange
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
C
      WCS00= (FLDCV(ICHAR,1)**2)*(FLDCV(JCHAR,1)**2)
      WCS00= AEPAR**2/SINW2**2 / (32*APIGR*AMSSN**4) * WCS00
      WCS00= WCS00*APCHI/DSQRT(ASVAR)
C
      SNE001= AECHI*AECHJ+APCHI*(APCHI-DSQRT(ASVAR)*AAVAR/ABVAR)
      SNE001= SNE001/(AAVAR**2-ABVAR**2)
      SNE001= 2*APCHI**2/ABVAR**2 + SNE001
      SNE002= APCHI*(DSQRT(ASVAR)-2*APCHI*AAVAR/ABVAR)
      SNE002= SNE002*DLOG(DABS((AAVAR+ABVAR)/
     &                         (AAVAR-ABVAR)))
      SNE002= SNE002/2/ABVAR**2
C
      SNETOT= WCS00*(SNE001+SNE002)
C
      RETURN
      END
C*DK SNEUPD
      DOUBLE PRECISION FUNCTION SNEUPD(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Production term due to scalar neutrino exchange
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
C
      WCS00= (FLDCV(ICHAR,1)**2)*(FLDCV(JCHAR,1)**2)
C
      SNEUPD= (AMASI**2-ATVAR)*(AMASJ**2-ATVAR)
      SNEUPD= SNEUPD * WCS00
      SNEUPD= (AEPAR**2)*(ADSNU**2)*SNEUPD
      SNEUPD= SNEUPD/(SINW2**2)
      SNEUPD= SNEUPD/(64.*APIGR*(ASVAR**2))
C
      RETURN
      END
C*DK SWA
      DOUBLE PRECISION FUNCTION SWA1(XL,XK)
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C
      SWA10= CS-DS*(XL+XK)+AMSWU*AMSWU
      SWA11= 4*(AS-BS*XL)*XL
      SWA1 = SWA11/SWA10/SWA10
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION SWA2(XL,XK)
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C
      SWA20= CS-DS*(XL+XK)+AMSWU*AMSWU
      SWA21= 4*(AS-BS*XK)*XK
      SWA2 = SWA21/SWA20/SWA20
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION SWA3(XL,XK)
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C
      SWA30= CS-DS*(XL+XK)+AMSWU*AMSWU
      SWA31= 2*FS*(CS-DS*(XK+XL))
      SWA3 = SWA31/SWA30/SWA30
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION SWA4(XL,XK,LFERM)
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C
      IF(LFERM.EQ.1) THEN
        AMASU= AMSSN
        AMASD= AMSLL
      ELSE IF(LFERM.EQ.2) THEN
        AMASU= AMSUL
        AMASD= AMSDL
      END IF
C
      SWA40= BS-DS*XK-AMASD*AMASD
      SWA41= 4*(AS-BS*XK)*XK
      SWA4 = SWA41/SWA40/SWA40
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION SWA5(XL,XK,LFERM)
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C
      IF(LFERM.EQ.1) THEN
        AMASU= AMSSN
        AMASD= AMSLL
      ELSE IF(LFERM.EQ.2) THEN
        AMASU= AMSUL
        AMASD= AMSDL
      END IF
C
      SWA50= BS-DS*XL-AMASU*AMASU
      SWA51= 4*(AS-BS*XL)*XL
      SWA5 = SWA51/SWA50/SWA50
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION SWA6(XL,XK,LFERM)
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C
      IF(LFERM.EQ.1) THEN
        AMASU= AMSSN
        AMASD= AMSLL
      ELSE IF(LFERM.EQ.2) THEN
        AMASU= AMSUL
        AMASD= AMSDL
      END IF
C
      SWA60= CS-DS*(XL+XK)+AMSWU*AMSWU
      SWA61= BS-DS*XL-AMASU*AMASU
      SWA62= 8*(AS-BS*XL)*XL
      SWA6 = SWA62/SWA61/SWA60
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION SWA7(XL,XK,LFERM)
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C
      IF(LFERM.EQ.1) THEN
        AMASU= AMSSN
        AMASD= AMSLL
      ELSE IF(LFERM.EQ.2) THEN
        AMASU= AMSUL
        AMASD= AMSDL
      END IF
C
      SWA70= CS-DS*(XL+XK)+AMSWU*AMSWU
      SWA71= BS-DS*XL-AMASU*AMASU
      SWA72= 2*FS*(CS-DS*(XL+XK))
      SWA7 = SWA72/SWA71/SWA70
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION SWA8(XL,XK,LFERM)
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C
      IF(LFERM.EQ.1) THEN
        AMASU= AMSSN
        AMASD= AMSLL
      ELSE IF(LFERM.EQ.2) THEN
        AMASU= AMSUL
        AMASD= AMSDL
      END IF
C
      SWA80= CS-DS*(XL+XK)+AMSWU*AMSWU
      SWA81= BS-DS*XK-AMASD*AMASD
      SWA82= 2*FS*(CS-DS*(XL+XK))
      SWA8 = SWA82/SWA81/SWA80
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION SWA9(XL,XK,LFERM)
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C
      IF(LFERM.EQ.1) THEN
        AMASU= AMSSN
        AMASD= AMSLL
      ELSE IF(LFERM.EQ.2) THEN
        AMASU= AMSUL
        AMASD= AMSDL
      END IF
C
      SWA90= CS-DS*(XL+XK)+AMSWU*AMSWU
      SWA91= BS-DS*XK-AMASD*AMASD
      SWA92= 8*(AS-BS*XK)*XK
      SWA9 = SWA92/SWA91/SWA90
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION SWA0(XL,XK,LFERM)
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C
      IF(LFERM.EQ.1) THEN
        AMASU= AMSSN
        AMASD= AMSLL
      ELSE IF(LFERM.EQ.2) THEN
        AMASU= AMSUL
        AMASD= AMSDL
      END IF
C
      SWA00= BS-DS*XL-AMASU*AMASU
      SWA01= BS-DS*XK-AMASD*AMASD
      SWA02= 2*FS*(CS-DS*(XL+XK))
      SWA0 = SWA02/SWA01/SWA00
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION SWA(IOP,LT,LD1,LD2,NCH,N0)
C-------------------------------------------------------------------
C IOP = 1   CHI+ --> CHI0 X
C IOP = 2   CHI0 --> CHI+ X
C Output  IOP=-1 means "Not enough time to do the job"
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
C*CA FANDCC
      COMMON /FANDCC/ AS,BS,CS,DS,FS,ES,GS,
     +                OL,OR,CL,CR,
     +                W1,W2
C*CC FANDCC
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C*CA MASSES
      COMMON /MASSES/ XM1,XM2,XM3
C*CC MASSES
      EXTERNAL FTEST
      INTEGER  FTEST
      REAL R1,R2
C
C----- First : check if you have enough time
      CALL TIMEL(TREM)
      IF(TREM.LE.10.) THEN
        IOP= -1
        RETURN
      END IF
C
      NRMAX= 100*NINTE
      NGMAX= NINTE
C
      CALL CHDECS(IOP,LD1,LD2,NCH,N0,IER)
      IF(IER.NE.0) THEN
        SWA= 0.D0
        RETURN
      END IF
C
      LF= 1
      IF(LD1.GE.4) LF= 2
C
      NRAN= 0
      NINT= 0
      SWA= 0.D0
      GX0= -100000.D0
789   CONTINUE
      NRAN= NRAN+1
      IF(NRAN.GT.NRMAX) GOTO 10
      R1= RNDM(R1)
      X1= ALIM*R1
      AM20= ALIM-X1
      AM21= (AMDEC(1)-AMDEC(4)*AMDEC(4)/(AMDEC(1)-2.*X1))/2.D0
      X1= X1/AMDEC(1)
      R2= RNDM(R2)
      X2= AM20 + (AM21-AM20)*R2
      X2= X2/AMDEC(1)
      J= FTEST(X1,X2,C12)
      IF(J.EQ.0) GOTO 789
      NINT= NINT + 1
      X1= X1*AMDEC(1)
      X2= X2*AMDEC(1)
C
      IF(LT.EQ.0) THEN
        GX= SWADIF(X1,X2,LF)
      ELSE IF(LT.EQ.1) THEN
        GX= SWA1(X1,X2)   *ECP(1,1)
      ELSE IF(LT.EQ.2) THEN
        GX= SWA2(X1,X2)   *ECP(2,1)
      ELSE IF(LT.EQ.3) THEN
        GX= SWA3(X1,X2)   *ECP(3,1)
      ELSE IF(LT.EQ.4) THEN
        GX= SWA4(X1,X2,LF)*ECP(4,1)
      ELSE IF(LT.EQ.5) THEN
        GX= SWA5(X1,X2,LF)*ECP(5,1)
      ELSE IF(LT.EQ.6) THEN
        GX= SWA6(X1,X2,LF)*ECP(6,1)
      ELSE IF(LT.EQ.7) THEN
        GX= SWA7(X1,X2,LF)*ECP(7,1)
      ELSE IF(LT.EQ.8) THEN
        GX= SWA8(X1,X2,LF)*ECP(8,1)
      ELSE IF(LT.EQ.9) THEN
        GX= SWA9(X1,X2,LF)*ECP(9,1)
      ELSE IF(LT.EQ.10) THEN
        GX= SWA0(X1,X2,LF)*ECP(10,1)
      END IF
      SWA= SWA + GX
      IF(GX.GT.GX0) GX0= GX
C
      IF(NINT.LT.NINTE) GOTO 789
10    CONTINUE
      IF(NINT.EQ.0) THEN
        LT= -2
        SWA= 0.D0
        RETURN
      END IF
C
      UFACT= AEPAR/SINW2
      UFACT= UFACT*UFACT/64./APIGR/APIGR/APIGR
      UFACT= UFACT/AMDEC(1)
      SWA= SWA*UFACT/DFLOAT(NINT)
      XRAT= DFLOAT(NINT)/DFLOAT(NRAN)
      XSUR(LD1,LD2)= XRAT
      WEIMAX(LD1,LD2)= GX0
C
      SWA= SWA*XRAT*FSUR(AMDEC(1),AMDEC(4))
      IF(IDEB.EQ.2) THEN
        WRITE(*,8) (ECP(K,1),K=1,3),LD1,LD2,XSUR(LD1,LD2),SWA
      END IF
8     FORMAT(1X,3(2X,E9.2),/,1X,2I6,2(4X,E10.3))
C
      RETURN
      END
C*DK SWADIF
      DOUBLE PRECISION FUNCTION SWADIF(X1,X2,LF)
C-------------------------------------------------------------------
C!Chi+ decay spectrum
C X1= anti-fermion DOWN energy, X2= fermion UP energy
C-------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANCOU
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
C*CC FANCOU
C
      GX= 0.D0
      GX= SWA1(X1,X2)   *ECP(1,1)
      GX= SWA2(X1,X2)   *ECP(2,1)  + GX
      GX= SWA3(X1,X2)   *ECP(3,1)  + GX
      GX= SWA4(X1,X2,LF)*ECP(4,1)  + GX
      GX= SWA5(X1,X2,LF)*ECP(5,1)  + GX
      GX= SWA6(X1,X2,LF)*ECP(6,1)  + GX
      GX= SWA7(X1,X2,LF)*ECP(7,1)  + GX
      GX= SWA8(X1,X2,LF)*ECP(8,1)  + GX
      GX= SWA9(X1,X2,LF)*ECP(9,1)  + GX
      GX= SWA0(X1,X2,LF)*ECP(10,1) + GX
C
      SWADIF= GX
C
      RETURN
      END
C*DK TERMIN
      SUBROUTINE TERMIN
C-----------------------------------------------------------------
C! Close the generator
C-----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C
      IF(IDEB.EQ.2) THEN
        CALL LUEEVT(10,0.)
      END IF
      IF(IPAW.EQ.1) THEN
C--- Histo file for PAW
        CALL HRPUT(0,'CHA001 HIST A','N')
      END IF
C
      IF(NEVENT(7).GT.0) THEN
        WRITE(LUNOUT,100) NEVENT(7)
      END IF
100   FORMAT(//,1X,I10,
     & ' DECAYS HAD NOT ENOUGH ENERGY FOR LUND FRAGMENTATION',//)
C
      RETURN
      END
C*DK TWOBOD
      DOUBLE PRECISION FUNCTION TWOBOD(AM0,AM1,AM2,CL,CR)
C----------------------------------------------------------------
C! Width AM0 --> AM1 AM2
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C
      IF(AM0.LE.(AM1+AM2)) THEN
        TWOBOD= 0.D0
        RETURN
      END IF
C
      WK1= AM0*AM0+AM1*AM1-AM2*AM2
      WK1= (CL*CL+CR*CR)*WK1
      WK2= 4.D0*AM0*AM1*CL*CR
      WK= AEPAR*(WK1+WK2)/(2.D0*SINW2)
C
      WE1= (AM0*AM0+AM1*AM1-AM2*AM2)/2.D0/AM0
      WP= DSQRT(WE1*WE1-AM1*AM1)
C
      TWOBOD= WK*WP/(8.D0*APIGR*AM0*AM0)
C
      RETURN
      END
C*DK TWOMES
      SUBROUTINE TWOMES(ICH,I1,I2,QFFB)
C----------------------------------------------------------------
C! Fill /LUJETS/ with two-mesons final states
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANINV
      COMMON /FANINV/ XINV(4,4),AMDEC(4),ALIM,
     +                WM0,WM1,WM2,WG0,WG1,WG2
C*CC FANINV
C*CA FANBRA
      DIMENSION RFER(6),XMFE(6),SIRF(6)
      DIMENSION WEIMAX(6,6),XBR(3),BTOP(3)
      DIMENSION XBR2(6,6,2),GA2F(6,6,2)
      DIMENSION XBRF(6,6),GAMF(6,6)
      COMMON /FANBR1/ ALPHA,GAM,GA2,XMTO,LNL
      COMMON /FANBR2/ RFER,XMFE,SIRF
      COMMON /FANBR3/ XBR,BTOP,XBRF,GAMF,XBR2,GA2F
      COMMON /FANBR4/ WEIMAX
C*CC FANBRA
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C*CA FORVEC
      COMMON /FORVEC/ QINI(5,3),QFIN(5,8)
C*CC FORVEC
C*CA LUNDCOM
      PARAMETER (L1MST=40, L1PAR=80)
      PARAMETER (L2KTYP=120, L2PMAS=120, L2PWID=60, L2KFR=80, L2CFR=40)
      PARAMETER (L3DPAR=20, L3IDB=120, L3CBR=400, L3KDP=1600)
      PARAMETER (L4CHAG=50, L4CHAF=100)
      PARAMETER (LEMSTE=40, LEPARE=80)
      PARAMETER (LJNPAR=2000)
      COMMON /LUDAT1/   MSTLU1(L1MST),PARLU1(L1PAR)
      COMMON /LUDAT2/   KTYPL2(L2KTYP),PMASL2(L2PMAS),PWIDL2(L2PWID)
     &                , KFRLU2(L2KFR),CFRLU2(L2CFR)
      COMMON /LUDAT3/   DPARL3(L3DPAR),IDBLU3(L3IDB),CBRLU3(L3CBR)
     &                , KDPLU3(L3KDP)
      COMMON /LUDAT4/   CHAGL4(L4CHAG),CHAFL4(L4CHAF)
      CHARACTER*4 CHAGL4,CHAFL4
      COMMON /LUDATE/  MSTELE(LEMSTE),PARELE(LEPARE)
      COMMON /LUJETS/  NPARLU,KODELU(LJNPAR,2),PARTLU(LJNPAR,5)
      REAL PARLU1,
     &     PMASL2,PWIDL2,CFRLU2,
     &     DPARL3,CBRLU3,
     &     PARELE,PARTLU
C*CC LUNDCOM
      PARAMETER( B0MAS= 5.2794, BCMAS= 5.2776 )
      DIMENSION QFFB(5),QW1(5),QW2(5)
      REAL R1
C
C     WRITE(LUNOUT,*)
C    &' _TWOMES_ : Not enough energy to fragmentate'
C     WRITE(LUNOUT,*)
C    &'            Two mesons created',ICH,I1,I2
      NEVENT(7)= NEVENT(7)+1
C----- Get flavours
      CALL GETIFL(ICH,I1,I2,IFL1,IFL2)
C
      AMFFB= QFFB(5)
      IF(IABS(IFL1).LT.100) THEN
        QW1(5)= PMASL2(IABS(IFL1))
      ELSE IF(IABS(IFL1).EQ.101) THEN
        QW1(5)= BCMAS
      ELSE IF(IABS(IFL1).EQ.102) THEN
        QW1(5)= B0MAS
      END IF
      IF(IABS(IFL2).LT.100) THEN
        QW2(5)= PMASL2(IABS(IFL2))
      ELSE IF(IABS(IFL2).EQ.101) THEN
        QW2(5)= BCMAS
      ELSE IF(IABS(IFL2).EQ.102) THEN
        QW2(5)= B0MAS
      END IF
      IF((QW1(5)+QW2(5)).GT.QFFB(4)) THEN
        WRITE(LUNOUT,*)
     & ' _TWOMES_: Not enough energy _ Do nothing',
     &  QW1(5),QW2(5),QFFB(4)
        RETURN
      END IF
C
      E1CMS= AMFFB*AMFFB+QW1(5)*QW1(5)-QW2(5)*QW2(5)
      E1CMS= E1CMS/2./AMFFB
      PCMS= E1CMS*E1CMS-QW1(5)*QW1(5)
      IF(PCMS.LT.(1.D-6)) PCMS= 1.D-6
      PCMS= DSQRT(PCMS)
C--- Choose direction
      CT= RNDM(R1)*2.-1.
      ST= DSQRT(1.-CT*CT)
      PH= 2.*APIGR*RNDM(PH)
      QW1(1)= PCMS*ST*DCOS(PH)
      QW1(2)= PCMS*ST*DSIN(PH)
      QW1(3)= PCMS*CT
      QW1(4)= E1CMS
      QW2(1)=-QW1(1)
      QW2(2)=-QW1(2)
      QW2(3)=-QW1(3)
      QW2(4)= AMFFB-QW1(4)
C----- Lorentz transf to lab
      PAX= QFFB(3)
      AMS= QFFB(5)
      CALL LORAX(2,PAX,AMS,QW1)
      CALL LORAX(2,PAX,AMS,QW2)
C----- Rotation
      CTH= QFFB(1)
      THE= APIGR/2.-DACOS(CTH)
      CALL ROTAX(1,THE,QW1)
      CALL ROTAX(1,THE,QW2)
      PHI= QFFB(2)-APIGR/2.
      CALL ROTAX(3,PHI,QW1)
      CALL ROTAX(3,PHI,QW2)
C
C----- Fill LUJETS
      K0= NPARLU
      KODELU(K0+1,1)= 3 + ICH
      KODELU(K0+2,1)= 3 + ICH
      KODELU(K0+1,2)= IFL1
      KODELU(K0+2,2)= IFL2
      DO 10 L1=1,5
        PARTLU(K0+1,L1)= SNGL(QW1(L1))
        PARTLU(K0+2,L1)= SNGL(QW2(L1))
10    CONTINUE
      NPARLU= NPARLU+2
C
      RETURN
      END
C*DK USCJOB
      SUBROUTINE USCJOB
C --------------------------------------------------------------------
C!End of generation
C --------------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C
C End of generation
C
      CALL TERMIN
C
C Print event counters
C
       WRITE(LUNOUT,101)
  101  FORMAT(//20X,'EVENTS STATISTICS',
     &         /20X,'*****************')
       WRITE(LUNOUT,102) NEVENT(1),NEVENT(2),NEVENT(3)
  102  FORMAT(/5X,'# OF GENERATED EVENTS                      = ',I10,
     &        /5X,'# OF ACCEPTED  EVENTS                      = ',I10,
     &        /5X,'# OF REJECTED  EVENTS (ISTA # 0 in ASKUSE) = ',I10)
       WRITE(LUNOUT,103)
  103  FORMAT(//20X,'ERRORS STATISTICS',
     &         /20X,'*****************')
       WRITE(LUNOUT,104) NEVENT(4),NEVENT(5),NEVENT(6)
  104  FORMAT(/10X,'ISTA # 0 FROM KINMIN        # OF REJECT = ',I10,
     &        /10X,'ISTA # 0 FROM KXLUAL        # OF REJECT = ',I10,
     &        /10X,'XSPROD CALLED FROM CHAEVT:  # OF REJECT = ',I10)
      RETURN
      END
C*DK USKRIN
      SUBROUTINE USKRIN(ECM)
C--------------------------------------------------------------
C! Scan facility ausiliary routine: init. evt dep. variables
C  Author:  G. Ganis     1 July 1991
C
C  Input :  ECM  c.m. energy ( GeV )
C  Output:  none
C--------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
C*CA BCS
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2, LBCS=50000)
      COMMON / BCS / IW(LBCS )
      REAL RW(LBCS)
      EQUIVALENCE (RW(1),IW(1))
C*CC BCS
      INTEGER ALTABL
      EXTERNAL ALTABL
C----- Set ECM
      TABL(1)= ECM
      ENECM  = ECM
C
C----- Initialize initial state radiator
      CALL INIRAD(SIG,IER)
      IF(IER.EQ.-1) THEN
       WRITE(LUNOUT,*) ' Below threshold - STOP'
       CALL EXIT
      END IF
C
      NCOL = NTABL
      NROW = 1
      JKPAR = ALTABL('KPAR',NCOL,NROW,TABL,'2I,(F)','C')
      CALL PRTABL('KPAR',0)
C
C---- Initialization event counters
      DO 2 L1=1,7
       NEVENT(L1) = 0
2     CONTINUE
C
      RETURN
      END
C*DK XKSECT
      REAL FUNCTION XKSECT(ECM)
C--------------------------------------------------------------
C! Scan facility auxiliary function: cross section
C  Author:  G.Ganis            1 July 1991
C
C  Input :  ECM  c.m. energy ( GeV )
C  Output:  XKSECT cross section ( nbarn )
C--------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA GPARAM
      INTEGER LDC(2,2),NEVENT(7),ITYP(2)
      PARAMETER( NTABL= 19 )
      REAL SDVRT(3),VERTX(4),TABL(NTABL)
      DOUBLE PRECISION YMAX
      COMMON /GPARAM/ LDC,KEYRAD,NEVENT,YMAX,ITYP,
     &                SDVRT,VERTX,LUCH2,LUNE0,LUSNU
     &               ,TABL
C*CC GPARAM
      EXTERNAL CHASIG
C
      IF(KEYRAD.EQ.0) THEN
        SS= ECM*ECM
        XS= CHASIG(SS)
      ELSE
        EBEAM= ECM/2.
        STHR= 4.D0*AMSCH(2)*AMSCH(2)
        INDEX= 1
        IPRI = 0
        CALL REMT1(EBEAM,CHASIG,STHR,INDEX,SIG1,IPRI)
        XS= SIG1
        IF(IPRI.EQ.-1) THEN
          WRITE(LUNOUT,*) ' _XKSECT_ Error during REMT1 call'
          CALL EXIT
        END IF
      END IF
      XKSECT= XS
C
      RETURN
      END
C*DK XSINTE
      DOUBLE PRECISION FUNCTION XSINTE(LSTEP,SQRTS,
     &                      COSMX,ICHAR,JCHAR,ITERM,YMAX)
C----------------------------------------------------------------
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      XSTEP= 2.*COSMX/LSTEP
      XSINTE= 0.0
      YMAX= -10000.D0
      DO 10 LSTE=1,LSTEP
        COSTH= -COSMX + (LSTE-1)*XSTEP
        Y1= XSPROD(SQRTS,COSTH,ICHAR,JCHAR,ITERM,ISTAT)
        IF(ISTAT.NE.0) THEN
          XSINTE= 0.0
          GOTO 999
        END IF
        COSTH= COSTH + XSTEP
        IF(Y1.GT.YMAX) YMAX= Y1
        Y2= XSPROD(SQRTS,COSTH,ICHAR,JCHAR,ITERM,ISTAT)
        IF(ISTAT.NE.0) THEN
          XSINTE= 0.0
          GOTO 999
        END IF
        IF(Y2.GT.YMAX) YMAX= Y2
        GSW= (Y2+Y1)/2.D0
        GX= COSTH-XSTEP/2.D0
        XSINTE= XSINTE + GSW*XSTEP
        CALL HF1(10010,GX,GSW)
C      WRITE(*,*) Y1,Y2,XSINTE
10    CONTINUE
C
999   CONTINUE
C
      RETURN
      END
C*DK XSPROD
      DOUBLE PRECISION FUNCTION XSPROD(SQRTS,COSTH,
     &                         ICHAR,JCHAR,ITERM,ISTAT)
C----------------------------------------------------------------
C!Chargino production differential cross-section
C Input  :  SQRTS  CMS energy
C           COSTH  cosinus of the angle between e+ and chi+
C           ICHAR  chi+ index ( 1 heaviest, 2 lightest )
C           JCHAR  chi- index ( 1 heaviest, 2 lightest )
C           ITERM  1 --> photon*
C                  2 --> Z0*
C                  3 --> scalar neutrino*
C                  4 --> total
C Output :  ISTAT  0 if o.k.
C
C All formulas are accordingly to :
C Bartl A. et al., Z. Phys. C , 30,441-449(1986)
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      ISTAT= 0
C
      AMSI= AMSCH(ICHAR)
      AMSJ= AMSCH(JCHAR)
      AECHI= (SQRTS*SQRTS+AMSI*AMSI-AMSJ*AMSJ)/2./SQRTS
      AECHJ= SQRTS-AECHI
      IF(AMSI.GT.AECHI.OR.AMSJ.GT.AECHJ)             THEN
C       WRITE(*,*) ' ++XSPROD++ : production not possible '
C       WRITE(*,*) ' En needed',AMSCH(ICHAR)+AMSCH(JCHAR),
C    +             ' En available',SQRTS
        ISTAT= 1
        RETURN
      END IF
      APCHI= DSQRT(AECHI*AECHI-AMSI*AMSI)
      APCHJ= APCHI
C
      ASVAR= SQRTS*SQRTS
      ATVAR= AMSCH(ICHAR)**2-SQRTS*(AECHI-APCHI*COSTH)
      AUVAR= AMSCH(JCHAR)**2-SQRTS*(AECHJ+APCHJ*COSTH)
C
      ADZDE= (ASVAR-AMSZ0**2)**2 + (AMSZ0*AGMZ0)**2
      AREDZ= (ASVAR-AMSZ0**2)/ADZDE
      AIMDZ= -AMSZ0*AGMZ0/ADZDE
C
      ADSNU= ATVAR - AMSSN**2
      ADSNU= 1./ADSNU
C
      IF(ITERM.EQ.1) THEN
        XSPROD= PHOTPD(ICHAR,JCHAR)*APCHI*SQRTS
      ELSE IF(ITERM.EQ.2) THEN
        XSPROD= ZED0PD(ICHAR,JCHAR)*APCHI*SQRTS
      ELSE IF(ITERM.EQ.3) THEN
        XSPROD= SNEUPD(ICHAR,JCHAR)*APCHI*SQRTS
      ELSE IF(ITERM.EQ.4) THEN
        XSPROD= PHOTPD(ICHAR,JCHAR)+
     +          ZED0PD(ICHAR,JCHAR)+
     +          SNEUPD(ICHAR,JCHAR)+
     +          PHZ0PD(ICHAR,JCHAR)+
     +          PHSNPD(ICHAR,JCHAR)+
     +          Z0SNPD(ICHAR,JCHAR)
        XSPROD= XSPROD * APCHI*SQRTS
      END IF
C
      RETURN
      END
C*DK XSTOT
      DOUBLE PRECISION FUNCTION XSTOT(SQRTS,
     &                         ICHAR,JCHAR,ITERM,ISTAT)
C----------------------------------------------------------------
C!Chargino production differential cross-section
C Input  :  SQRTS  CMS energy
C           ICHAR  chi+ index ( 1 heaviest, 2 lightest )
C           JCHAR  chi- index ( 1 heaviest, 2 lightest )
C           ITERM  1 --> photon*
C                  2 --> Z0*
C                  3 --> scalar neutrino*
C                  4 --> total
C Output :  ISTAT  0 if o.k.
C
C All formulas are accordingly to :
C Bartl A. et al., Z. Phys. C , 30,441-449(1986)
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      ISTAT= 0
      XSTOT= 0.D0
C
      AMSI= AMSCH(ICHAR)
      AMSJ= AMSCH(JCHAR)
      AECHI= (SQRTS*SQRTS+AMSI*AMSI-AMSJ*AMSJ)/2./SQRTS
      AECHJ= SQRTS-AECHI
      IF(AMSI.GE.AECHI.OR.AMSJ.GE.AECHJ)  THEN
C       WRITE(*,*) ' ++XSPROD++ : production not possible '
C       WRITE(*,*) ' En needed',AMSCH(ICHAR)+AMSCH(JCHAR),
C    +             ' En available',SQRTS
        ISTAT= 1
        RETURN
      END IF
      APCHI= DSQRT(AECHI*AECHI-AMSI*AMSI)
      APCHJ= APCHI
C
      ASVAR= SQRTS*SQRTS
      AAVAR= 2.*AMSSN*AMSSN+ASVAR-AMSI*AMSI-AMSJ*AMSJ
      AAVAR= AAVAR/2./AMSSN/AMSSN
      ABVAR= APCHI*SQRTS/AMSSN/AMSSN
      AHVAR= APCHI*AAVAR/ABVAR
      AHVAR= AECHI*AECHJ+AHVAR*AHVAR-APCHI*SQRTS*AAVAR/ABVAR
      AHVAR= AHVAR*DLOG(DABS((AAVAR+ABVAR)/(AAVAR-ABVAR)))
      AHVAR= 2.*APCHI*SQRTS-2.*APCHI*APCHI*AAVAR/ABVAR+AHVAR
C
      ADZDE= (ASVAR-AMSZ0**2)**2 + (AMSZ0*AGMZ0)**2
      AREDZ= (ASVAR-AMSZ0**2)/ADZDE
      AIMDZ= -AMSZ0*AGMZ0/ADZDE
C
      IF(ITERM.EQ.1) THEN
        XSTOT= PHOTOT(ICHAR,JCHAR)
      ELSE IF(ITERM.EQ.2) THEN
        XSTOT= ZEDTOT(ICHAR,JCHAR)
      ELSE IF(ITERM.EQ.3) THEN
        XSTOT= SNETOT(ICHAR,JCHAR)
      ELSE IF(ITERM.EQ.4) THEN
        XSTOT= PHOTOT(ICHAR,JCHAR)+
     +         ZEDTOT(ICHAR,JCHAR)+
     +         SNETOT(ICHAR,JCHAR)+
     +         PHZTOT(ICHAR,JCHAR)+
     +         PSNTOT(ICHAR,JCHAR)+
     +         ZSNTOT(ICHAR,JCHAR)
      END IF
C
      RETURN
      END
C*DK Z0SNPD
      DOUBLE PRECISION FUNCTION Z0SNPD(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Inteference term : Z0-scalar neutrino
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
C
      WCS00= CFELF(2)/2.*FLDCV(ICHAR,1)*FLDCV(JCHAR,1)
C
      Z0SNP1= (AMASI**2-ATVAR)*(AMASJ**2-ATVAR)
      Z0SNP1= CO1LF(ICHAR,JCHAR)*Z0SNP1
      Z0SNP2= AMASI*AMASJ*ASVAR*ETACH(ICHAR)*ETACH(JCHAR)
      Z0SNP2= CO1RG(ICHAR,JCHAR)*Z0SNP2
C
      Z0SNPD= Z0SNP1 + Z0SNP2
      Z0SNPD= Z0SNPD * WCS00
      Z0SNPD= (AEPAR**2)*AREDZ*ADSNU*Z0SNPD
      Z0SNPD= Z0SNPD/((SINW2**2)*(1.-SINW2))
      Z0SNPD= Z0SNPD/(16.*APIGR*(ASVAR**2))
C
      RETURN
      END
C*DK ZED0PD
      DOUBLE PRECISION FUNCTION ZED0PD(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Production term due to Z0 exchange
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
C
      WCS01= CO1LF(ICHAR,JCHAR)**2 + CO1RG(ICHAR,JCHAR)**2
      WCS01= (CFELF(2)**2+CFERG(2)**2)/4. * WCS01
      WCS02= 4.*CO1LF(ICHAR,JCHAR)*CO1RG(ICHAR,JCHAR)
      WCS02= ETACH(ICHAR)*ETACH(JCHAR) * WCS02
      WCS02= (CFELF(2)**2+CFERG(2)**2)/4. * WCS02
      WCS03= CO1LF(ICHAR,JCHAR)**2 - CO1RG(ICHAR,JCHAR)**2
      WCS03= (CFELF(2)**2-CFERG(2)**2)/4. * WCS03
C
      ZED0P1= (AMASI**2-AUVAR)*(AMASJ**2-AUVAR)
      ZED0P1= (AMASI**2-ATVAR)*(AMASJ**2-ATVAR) + ZED0P1
      ZED0P1= ZED0P1 * WCS01
      ZED0P2= AMASI*AMASJ*ASVAR * WCS02
      ZED0P3= (AMASI**2-AUVAR)*(AMASJ**2-AUVAR)
      ZED0P3=-(AMASI**2-ATVAR)*(AMASJ**2-ATVAR) + ZED0P3
      ZED0P3=-ZED0P3 * WCS03
C
      ZED0PD= ZED0P1 + ZED0P2 + ZED0P3
      ZED0PD= ZED0PD/(ADZDE*(SINW2*(1-SINW2))**2)
      ZED0PD= ZED0PD*(AEPAR**2)
      ZED0PD= ZED0PD/(32.*(ASVAR**2)*APIGR)
C
      RETURN
      END
C*DK ZEDTOT
      DOUBLE PRECISION FUNCTION ZEDTOT(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Production term due to Z0 exchange
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
C
      WCS01= CO1LF(ICHAR,JCHAR)**2 + CO1RG(ICHAR,JCHAR)**2
      WCS01= (CFELF(2)**2+CFERG(2)**2)/4. * WCS01
      WCS02= 2.*CO1LF(ICHAR,JCHAR)*CO1RG(ICHAR,JCHAR)
      WCS02= ETACH(ICHAR)*ETACH(JCHAR) * WCS02
      WCS02= (CFELF(2)**2+CFERG(2)**2)/4. * WCS02
C
      ZED0P1= (AECHI*AECHJ+APCHI*APCHI/3.) * WCS01
      ZED0P2= AMASI*AMASJ * WCS02
C
      ZEDTOT= ZED0P1 + ZED0P2
      ZEDTOT= ZEDTOT/(ADZDE*(SINW2*(1-SINW2))**2)
      ZEDTOT= ZEDTOT*(AEPAR**2)
      ZEDTOT= ZEDTOT/(8.*APIGR)*APCHI/DSQRT(ASVAR)
C
      RETURN
      END
C*DK ZSNTOT
      DOUBLE PRECISION FUNCTION ZSNTOT(ICHAR,JCHAR)
C----------------------------------------------------------------
C!Inteference term : Z0-scalar neutrino
C----------------------------------------------------------------
C*CA FANJOB
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /FANJOB/ IDEB,LUNOUT,NINTE,IPAW,
     &                IDEB1,IDEB2,IHAND
C*CC FANJOB
C*CA FANPAR
      PARAMETER( AEPAR= 0.0917 )
      PARAMETER( APIGR= 3.14159265 )
      COMMON /FIXPAR/ AMSZ0,AGMZ0,SINW2,AMSWU,ENECM,
     +                IDELT(2,2),
     +                XFACT,AGMSF

      COMMON /VARPAR/ AV1V2,AMSCM,AMSMU,AMSM0,AMH10
      COMMON /FIELDS/ AMSCH(2),AMSNE(4),AMSN0,
     +                ETACH(2),ETANE(4),IMSNE(4),
     +                FLDCU(2,2),FLDCV(2,2),
     +                FLDNE(4,4)
      COMMON /FANSFE/ AMSSN,AMSLR,AMSLL,
     +                AMSUR,AMSUL,AMSDR,AMSDL,
     +                AMMIN
      COMMON /FANFER/ AMFER(6,2)
      COMMON /FANCOU/ CO1RG(2,2),CO1LF(2,2),
     +                CCHAX(2,2),CCHVC(2,2),
     +                CFERG(4),CFELF(4),
     +                CFFRG(4,4),CFFLF(4,4),
     +                CHH(4,4),
     +                COORG(4,2),COOLF(4,2),
     +                CO2RG(4,4),CO2LF(4,4),
     +                CNEAX(4,4),CNEVC(4,4),
     +                ECP(10,2),
     +                CWNE(4,4),
     +                CNFR0(2,4),CNFR1(2,4),
     +                CNFL0(2,4),CNFL1(2,4),
     +                CQ2(4,4),CR2(4,4)
      COMMON /FANCO1/ ENC( 6),XSUR(6,6)

      COMMON /HIGMAS/ AMHCH,AMH30,AMH20,T2ALF,CALF,SALF
C*CC FANPAR
C*CA FANKIN
      COMMON /FANKIN/ ASVAR,ATVAR,AUVAR,
     +                AREDZ,AIMDZ,ADZDE,ADSNU,
     +                ABSVA,ABTVA,ABUVA,
     +                ADSLL,ADSUL,ADSDL,ADEWU,
     +                AECHI,AECHJ,APCHI,
     +                AAVAR,ABVAR,AHVAR,
     +                AMASI,AMASK
C*CC FANKIN
C
      AMASI= AMSCH(ICHAR)
      AMASJ= AMSCH(JCHAR)
C
      WCS00=-CFELF(2)/2.*FLDCV(ICHAR,1)*FLDCV(JCHAR,1)
      WCS00= AEPAR**2/SINW2**2/(1-SINW2) * WCS00
      WCS00= WCS00*AREDZ/(16*APIGR)/ASVAR
C
      ZSNTOT= DLOG(DABS((AAVAR+ABVAR)/(AAVAR-ABVAR)))
      ZSNTOT= ETACH(ICHAR)*ETACH(JCHAR)*AMASI*AMASJ*ZSNTOT
      ZSNTOT= CO1RG(ICHAR,JCHAR)*ZSNTOT
      ZSNTOT= CO1LF(ICHAR,JCHAR)*AHVAR + ZSNTOT
      ZSNTOT= ZSNTOT * WCS00
C
      RETURN
      END
