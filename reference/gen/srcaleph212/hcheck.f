           SUBROUTINE HCHECK(X,IDEAD)
C---------------------------------------------------
CKEY HCALDES HCAL  STATUS POINT / USER
C
C!   Check if a point is inside Hcal
C!
C!                        Authors: C.Capon,G.Catanesi
C!
C!   Input : X/R Coordinates in the Aleph R.S.
C!
C!   output: IDEAD/I = if 0 the point is inside
C!
C!--------------------------------------------------
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
           DIMENSION X(*)
C
        PHI=ATAN2(X(2),X(1))
        IF (PHI.LT.0.) PHI=PHI+TWOPI
C?              go to local coordinates : yl is // to barrel module dept
        YL=X(1)*COS(PHI)+X(2)*SIN(PHI)
        XL=X(1)*SIN(PHI)-X(2)*COS(PHI)
        Z=X(3)
        ZABS=ABS(Z)
C
C?              test if point is inside Hcal
C
           IDEAD=0
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
C
           RETURN
           END
