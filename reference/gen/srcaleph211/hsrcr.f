        SUBROUTINE HSRCR(SYSTM,IT,JF,KS,LHPOI,CORNR)
C------------------------------------------------------------------
CKEY HCALDES HCAL TOWER CORNERS  / USER
C! i,j,k to storey corners conversion
C!
C!      G.Capon               date : 861001
C!   Subroutine to compute coordinates of storey corners
C!     from storey indices IT,JF,NR,KS
C! input :
C!         SYSTM : reference system (not used up to now)
C!           IT  : theta index of storey
C!           JF  : phi     "        "
C!           KS  : stack number
C!          LHPOI: number of storey points (=8 in general case)
C! output:
C!     CORNR(3,K): x,y,z coord of corner K (K=1,LHPOI) in Aleph syst
C!
C------------------------------------------------------------------
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
C!    GEOMETRY COMMONS FOR HADRON CALORIMETER
      PARAMETER( LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER( LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER( LPHCT = 4)
      PARAMETER( LPHCBM = 24,LPHCES = 6)
      COMMON /HCALGE/HCRMIN(LSHC),HCRMAX(LSHC),HCZMIN(LSHC),HCZMAX(LSHC)
     &              , NHCSUB,NHCTWR,NHCPHC,NHCREF,IHCTID(LHCRE-1)
     &              , HCTUTH,HCIRTH,HCLSLA,NHCPLA(LPHC),HCTIRF(LPHC)
     &              , HCSMTH
      COMMON / HBAR / NHCBMO,NHCBFS,NHCBBS,NHCBLA,HCLTNO(LHCNO)
     &              , HCWINO(LHCNO),HCDEWI,NHBLA2,NHBLI3,NHBLO3
     &              , NEITHC(LHCNL),NEITSP(LHCNL,LHCSP)
     &              , HCSPLT(LHCNL,LHCSP), HFSPBL,HFSRBL,HCPHOF
C
      COMMON /HEND/  HCDREC,HCDSTP,HCAPSL,HFSPEC,NHCSEX
     &           ,NHCEFS,NHCEBS,NHCTRE,NHCINL,NHCOUL
     &           ,NHCIND,NHCOUD
C
       PARAMETER (LHCBL=4,LHCEI=10,LHCEO=20,LHNLA=4)
      COMMON /HCCONS/ HCTHRF,HCRSIZ,HCZSIZ,NHCBAR,NHCECA
     &               ,NHCEIT,HCEIWI,HCDOWI,HCTUGA,HCSEPO
     &               ,HCSABL,HCSAEC,HCTUEN,XLNHCE(LHCBL)
     &               ,HCTLEI(LHNLA,LHCEI),HCTLEO(LHNLA,LHCEO)
     &               ,HCTAEI(LHCEI),HCTAEO(LHCEO),HTINBL,HTINEC(2)
     &               ,HTPIEC,HTPOEC,HBWREC,HBWCEC(2),HBSREC
     &               ,HBSCEC,HBWRBL,HBSCBL,NHMBDF(2),NHTY4D
     &               ,NHTY3D,NHTY2D,NHMBFL(2),NHBDOU,NHDLEC
     &               ,NHDET0,NHDEBS,NHL8EC(LHCNL-1),HCTUSH,XHCSHI(LHCBL)

      PARAMETER (LHCTR1=LHCTR+1)
      COMMON /HCSEVA/ NTHCFI(LHCRE),HCAPDE(LPHCT),HCFITW(LHCRE)
     &               ,HCBLSP(LHCNL,LHCSP),NHCTU1(LHCNL),HCTHUL(LHCTR1)
     &               ,PHCTOR(LHCTR),IHCREG(LHCTR)
     &               ,HCLARA(LHCNL),HCLAWI(LHCNL)
     &               ,YBAST1,YBARMX,ZENST1,ZENDMX
     &               ,XBARR0,XENDC0
C
C
C
        CHARACTER* (*) SYSTM (*)
        REAL YL(2),TP(2),FIL(2),ZE(2),CORNR(3,*)
        INTEGER JTBAR(3)
        DATA JTBAR/14,16,15/
C
        NR=IHCREG(IT)
        TP(1)=HCTHUL(IT)
        TP(2)=HCTHUL(IT+1)
        NPHI=12*(2**NR)
        DPHI=TWOPI/FLOAT(NPHI)
        NSTMO=NPHI/24
        JMOD=(JF-1)/NSTMO+1
        FIMOD=PIBY6*FLOAT(JMOD/2)
        FIL(1)=DPHI*FLOAT(JF-1)-FIMOD
        FIL(2)=FIL(1)+DPHI
        SINF=SIN(FIMOD)
        COSF=COS(FIMOD)
C
C?         set values for limiting planes
C?         y=const for barrel , z= const for endcap
C?         1 : inner storey face , 2 : outer storey face
C
        SIGNZ=1.
        JTETA=IT
        IF (IT.GT.LHCTR/2) THEN
           SIGNZ=-1.
           JTETA=LHCTR1-IT
           ENDIF
        IF(KS.EQ.1) THEN
           YL(1)=HCRMIN(1)
           YL(2)=YBAST1
           ZE(1)=HCZMIN(2)
           ZE(2)=ZENST1
           IF(JTETA.GE.10.AND.JTETA.LE.13)ZE(1)=HCZMIN(3)
           ELSE
           YL(1)=YBAST1
           YL(2)=YBARMX
           ZE(1)=ZENST1
           ZE(2)=ZENDMX
           IF(JTETA.GE.15.AND.JTETA.LE.17) YL(2)=HCRMAX(3)
           IF(JTETA.EQ.18) ZE(2)=HCZMAX(1)
           ENDIF
C
C?       intersect planes with straight lines
C?       phi = const , theta(proj) = const
C
        DO 10 NT=1,2
        DO 10 NF=1,2
        DO 10 IFACE=1,2
        N=NT+2*(NF-1)+4*(IFACE-1)
        LEVEL=KS+IFACE-1
        INTBA=0
        IF(JTETA.GE.JTBAR(LEVEL)) INTBA=1
        IF(LEVEL.EQ.3.AND.JTETA.EQ.18) INTBA=0
        IF (INTBA.GT.0) THEN
C?         intersections for barrel geometry
           XL=-YL(IFACE)*TAN(FIL(NF))
           CORNR(1,N)= XL*SINF+YL(IFACE)*COSF
           CORNR(2,N)=-XL*COSF+YL(IFACE)*SINF
           CORNR(3,N)=YL(IFACE)*COS(TP(NT))/SIN(TP(NT))
           ELSE
           ZZ=ZE(IFACE)*SIGNZ
C?            intersections for endcap geometry
           YLL=ZZ*TAN(TP(NT))
           XL=-YLL*TAN(FIL(NF))
           CORNR(1,N)= XL*SINF+YLL*COSF
           CORNR(2,N)=-XL*COSF+YLL*SINF
           CORNR(3,N)= ZZ
           ENDIF
   10    CONTINUE
C
C
        RETURN
        END
