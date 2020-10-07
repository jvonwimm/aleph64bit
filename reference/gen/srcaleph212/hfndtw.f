        SUBROUTINE HFNDTW(X,SYSRF,IT,JF,KS,ISUBC,IMOD,IPL,IDEAD)
C-------------------------------------------------------------------
CKEY HCALDES HCAL POINT TOWER  / USER
C! x,y,z to i,j,k conversion for Hcal
C!
C!      G.Capon               date : 861001
C!  convert x,y,z coordinates to storey indices and parameters
C!
C! input :
C!          X(3) : point coordinates (in Aleph system)
C!        SYSRF  : char. variable - specifies ref system - not used
C! output :
C!           IT  : theta index of storey
C!           JF  : phi     "        "
C!           KS  : stack number
C!          ISUBC: subcomp # (1=endcapA, 2=barrel, 3=endcapB)
C!          IMOD : physical module # (1-6 for endcaps,1-24 for barrel)
C!          IPL  : plane # (set to zero in this version)
C!         IDEAD : dead zone flag (if ne 0 point is outside Hcal)
C!
C-------------------------------------------------------------------
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
        REAL X(*)
        CHARACTER* (*) SYSRF (*)
C
CX      IF (SYSRF.NE.'ALEPH') RETURN
C
        IT = 0
        JF = 0
        KS = 0
        ISUBC = 0
        IMOD = 0
        IPL = 0
        IDEAD = 0
C
        PHI=ATAN2(X(2),X(1))
        IF (PHI.LT.0.) PHI=PHI+TWOPI
        KK=INT((PHI+PIBY12)/PIBY6)
        PHIC=FLOAT(KK)*PIBY6
C?              go to local coordinates : yl is // to barrel module dept
        YL=X(1)*COS(PHIC)+X(2)*SIN(PHIC)
        XL=X(1)*SIN(PHIC)-X(2)*COS(PHIC)
        Z=X(3)
        ZABS=ABS(Z)
C
C?              test if point is inside Hcal
C
           IF(ZABS.GT.ZENDMX)THEN
                   IDEAD=1
           ELSEIF(YL.GT.YBARMX)THEN
                   IDEAD=2
           ELSEIF(ZABS.GT.HCZMAX(1).AND.YL.GT.HCRMAX(3))THEN
                   IDEAD=3
           ELSEIF(YL.LT.HCRMIN(2))THEN
                   IDEAD=4
           ELSEIF(ZABS.LT.HCZMIN(2).AND.YL.LT.HCRMAX(2))THEN
                   IDEAD=5
          ELSEIF(HCRMAX(2).LT.YL.AND.YL.LT.HCRMIN(1)
     *    .AND.ZABS.LT.HCZMIN(3))THEN
                   IDEAD=6
           ELSEIF(HCZMAX(1).LT.ZABS.AND.ZABS.LT.HCZMIN(3))THEN
                   IDEAD=7
           ENDIF
           IF(IDEAD.GT.0) RETURN
C
C?              get ISUBC,IMOD,KS,IPL
C
        IF(ZABS.LT.HCZMAX(1).AND.YL.GT.HCRMIN(1))THEN
           ISUBC=2
           IMOD=PHI/PIBY12+1.
C?              IPL set to zero in this version
           IPL=0
           KS=1
           IF(YL.GT.YBAST1)KS=2
        ELSE
           ISUBC=1
           IF(Z.LT.0.)ISUBC=3
           IMOD=PHI/PIBY3+1.
           IPL=0
           KS=1
           IF(ZABS.GT.ZENST1)KS=2
        ENDIF
C
C?              get theta index
C
        TETAP=ATAN2(YL,Z)
        IF(TETAP.LT.HCTHUL(1))                                  GO TO 20
        DO 10 K=2,63
        IF(TETAP.LT.HCTHUL(K))                                  GO TO 30
   10    CONTINUE
C
C   point outside pads
   20    IDEAD = 8
         RETURN
C
   30    IT=K-1
C
C?              get region nr, phi index
C
        NR=IHCREG(IT)
        NPHI=12*(2**NR)
        DPHI=TWOPI/FLOAT(NPHI)
        JF=PHI/DPHI+1.
C
        RETURN
        END
