              REAL FUNCTION HTSTEC(ITUB,ILAY,IDHEI)
C--------------------------------------------------------------
CKEY HCALDES HCAL ENDCAP POSITION TUBE /USER
C
C!  Return the starting position of the tube ITUB in the EndCap
C!
C!                                  Author:G.Catanesi 14/06/89
C!
C!  INPUT:
C!           ITUB/I = tube#
C!           ILAY/I = layer#
C!           IDHEI/I= double_eightfold#
C!
C--------------------------------------------------------------
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
                HTSTEC = 0.
C
C iron wall + dead zone inside tube
                XDEAD = HCDRBM(LPECA)+HTPIEC-HTINEC(2)
C
C Double_eightfold at 0 degrees
                IF(IDHEI.EQ.1)THEN
                   HTSTEC = HCRMIN(2) + XDEAD + HBWCEC(2)
                ELSEIF(IDHEI.EQ.2)THEN
                      XDIFF  = HTXDTL(IDHEI,ILAY) -
     &                            HTXDTL(IDHEI-1,ILAY)
                   IF(MOD((ITUB-1),2*NHCEIT).GE.NHCEIT)THEN
                      XDIFF  = XDIFF + 2*NHCEIT*HTXSPL(IDHEI,ILAY)
                   ENDIF
                   HTSTEC = HCRMIN(2) + XDEAD + HBWCEC(2) - XDIFF
                ELSEIF(IDHEI.EQ.3)THEN
                   YGAP   = HCDRBM(LPECA) + NHDET0*HCDOWI
                   XGAP   = YGAP/TAN(PIBY3)
                   HTSTEC = XGAP + XDEAD +HBWCEC(2)
                ELSE
C  Double_eightfold at 30 degrees
                   YGAP   = HCDRBM(LPECA) +
     &                         INT((IDHEI-1)/NHDEBS)*(HCDSTP+HCAPSL) +
     &                         MOD((IDHEI-1),NHDEBS)*HCDOWI
                   XGAP   = YGAP/TAN(PIBY3)
                   HTSTEC = XDEAD + XGAP + HBWCEC(1)  +
     &                     MOD((ITUB-1),2*NHCEIT) * HTXSPL(IDHEI,ILAY)
                ENDIF
C
                RETURN
                END
