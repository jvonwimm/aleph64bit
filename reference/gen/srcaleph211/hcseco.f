      SUBROUTINE HCSECO
C------------------------------------------------------------------
CKEY HCALDES HCAL GEOM CONSTANT / INTERNAL
C                                                                 C
C! Calculate Geometrical quantities from the D.B. constants       C
C!                                  Author:G.Catanesi 87/10/10    C
C!                                                                C
C!          Called by:HCIRUN                                      C
C!          Calls    :HCBOUN                                      C
C!                                                                C
C!-----------------------------------------------------------------
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
C
C. Towers boundaries in theta
C.
      CALL HCBOUN
C
C.      lower edges of iron spacers in end caps (sextant Ref.Sys)
C.
      DO 10 JJ=1,LPHCT
         HCAPDE(JJ) = HCDREC + HCDSTP + (HCDSTP+HCAPSL)*(JJ-1)
   10 CONTINUE
C.
C.    number of columns in each tower region
C.
      DO 20 J=1,LHCRE
         NTHCFI(J) = 2**J
   20 CONTINUE
C.
C.   phi size of tower in region I
C.
      DO 30 I=1,LHCRE
         HCFITW(I) = PIBY6/NTHCFI(I)
   30 CONTINUE
C.
C   spacers position  and # of tubes  in layers (barrel)
C
      DO 40 N=1,LHCNL
         NHCTU1(N) = NEITHC(N) * NHCEIT
         HCBLSP(N,1) = HCDEWI + NEITSP(N,1)*HCEIWI
         HCBLSP(N,2) = HCDEWI + NEITSP(N,2)*HCEIWI + HCSPLT(N,1)
C
         IF(NEITSP(N,1).EQ.-1)HCBLSP(N,1) = 0.
         IF(NEITSP(N,2).EQ.-1)HCBLSP(N,2) = 0.
   40 CONTINUE
C.
C. Phi width in a tower row
C.
      DO 50 JJ=1,LHCTR
         KFAC = 1
         IF(JJ.LE.IHCTID(2).OR.JJ.GT.(LHCTR-IHCTID(2)))KFAC = 2
         IF(JJ.LE.IHCTID(1).OR.JJ.GT.(LHCTR-IHCTID(1)))KFAC = 4
         PHCTOR(JJ) = (TWOPI*KFAC)/NHCPHC
   50 CONTINUE
C.
C.   Layer radius and width (barrel)
C.
      TAN15 = TAN(PIBY12)
      HCLARA(1) = HFSRBL
      HCLAWI(1) = HFSRBL*TAN15
C
      DO 60 JJ=2,NHCBLA
         HCLARA(JJ) = HFSRBL + (JJ-1)*HCSMTH
         HCLAWI(JJ) = (HCLARA(JJ))*TAN15
   60 CONTINUE
C
      DO 80 J=1,NHCTWR/2
C
         IHCREG(J) = 1
         DO 70 K=1,NHCREF-1
            IF(J.GT.IHCTID(K))THEN
               IHCREG(J) = K+1
            ENDIF
   70    CONTINUE
         IHCREG(LHCTR+1-J) = IHCREG(J)
   80 CONTINUE
C
C?            get overall dimensions (for analog readout)
C
        YBAST1=HCRMIN(1)+FLOAT(NHCBFS-1)*(HCIRTH+HCTUTH)+HCTUTH+0.5
        YBARMX=HCRMAX(1)-HCLSLA
        ZENST1=HCZMIN(2)+FLOAT(NHCEFS)*(HCIRTH+HCTUTH)
        ZENDMX=HCZMAX(3)-HCLSLA
C
C?            get constants for digital readout
C
        XBARR0=HCDEWI+0.5*HCSABL
        XENDC0=HCDREC+0.5*HCSAEC
C
C
        RETURN
        END
