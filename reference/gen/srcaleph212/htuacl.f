           REAL FUNCTION HTUACL(ITUB,ILAY,IMOD,IPOR)
C-------------------------------------------------------------
CKEY HCALDES HCAL TUBE LENGTH /USER
C
C!   Return the active length of each tube in HCAL
C!   If 0. the tube is absent or dead
C!
C!                             Author : G.Catanesi 23/5/89
C!
C!         INPUT:
C!                 ITUB/I = Tube#
C!                 ILAY/I = Layer#
C!                 IMOD/I = Module#
C!                 IPOR/I = Portion#
C!
C----------------------------------------------------------------
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
         HTUACL = 0.
C
C   Evaluate eightfold and double_eightfold number
C
            CALL HNEIGH(ITUB,ILAY,IPOR,IHEIF,IDHEI)
            IF(IHEIF.EQ.0)GOTO 99
C
C
         IF(IPOR.EQ.LPBAR) THEN
C
C  Barrel Case
C
            IF(IMOD.NE.NHMBDF(1).AND.IMOD.NE.NHMBDF(2)) THEN
C
C  Standard module case
C
              IF((IHEIF.LE.NHBLI3) .OR.
     &             (IHEIF.GT.(NEITHC(ILAY)-NHBLO3)))THEN
                 HTUACL = XLNHCE(3)
              ELSEIF((IHEIF.GT.NHBLI3) .AND.
     &             (IHEIF.LE.(NHBLI3+NHBLA2)))THEN
                 HTUACL = XLNHCE(2)
              ELSE
                 HTUACL = XLNHCE(1)
              ENDIF
C
C  Kills last 2 eigthfold of first plane in modules 15 and 22
C
              IF((IMOD.EQ.NHMBFL(1).OR.IMOD.EQ.NHMBFL(2)) .AND.
     &         (ILAY.EQ.1))THEN
                  ILAST = NEITHC(ILAY) - NHBDOU
                  IF(IHEIF.GT.ILAST)GOTO 99
              ENDIF
C
           ELSE
C
C  Special case (modules 6 and 7)
C
              ISEC = NHTY4D + NHTY3D
              ITHR = ISEC + NHTY2D
              IF(IHEIF.LE.NHTY4D)THEN
                 HTUACL = XLNHCE(4)
              ELSEIF((IHEIF.GT.NHTY4D).AND.(IHEIF.LE.ISEC))THEN
                 HTUACL = XLNHCE(3)
              ELSEIF((IHEIF.GT.ISEC).AND.(IHEIF.LE.ITHR))THEN
                 HTUACL = XLNHCE(2)
              ELSEIF((IHEIF.GT.ITHR) .OR.
     &                 (IHEIF.GT.(NEITHC(ILAY)-NHBLO3)))THEN
                 HTUACL = XLNHCE(1)
              ELSE
                 HTUACL = XLNHCE(3)
              ENDIF
           ENDIF
C
         ELSE
C
C Endcap Case
C
C
C
C   Evaluate angle and length of the double_eightfold profile
C
            DTUBL = HTXDTL(IDHEI,ILAY)
            IF(DTUBL.EQ.0.)GOTO 99
            ANGLE = HTXANG(IDHEI,ILAY)
            STEP  = HTXSPL(IDHEI,ILAY)
C
C
C
C Calculates Tube_profile length
C
            IF (ANGLE.NE.0.) THEN
               ISN = SIGN(1.,ANGLE)
               IS  = 2
               IF(ISN.EQ.1)THEN
C
C Tube_profile length 30 degrees case
                  TUBL = DTUBL - (STEP/2. +
     &                  MOD((ITUB-1),(2*NHCEIT))*STEP)
               ELSE
C
C Tube_profile length -30 degrees case
                   IF(MOD(IHEIF,2).EQ.0)THEN
                         TUBL = HCTLEI(3,IDHEI)
                   ELSE
                      TUBL = DTUBL
                  ENDIF
               ENDIF
            ELSE
C
C Tube_profile length 0 degrees case
               IS  = 1
               TUBL = DTUBL
               IF((IHEIF.EQ.4) .AND. (ILAY.GT.NHCINL))
     &                TUBL = HCTLEO(3,IDHEI)
            ENDIF
C
C
C
C Evaluate non active zones at the boundaries of the tube profile
C
            XOUT = HTINEC(1)
            XINN = HTINEC(2)
C
C Subtract these zones to the tube-profile length
C
            HTUACL = TUBL - XOUT - XINN
            IF(HTUACL.LE.0.)GOTO 99


         ENDIF
C
         RETURN
C
99       CONTINUE
C
C The tube is absent or dead
C
         HTUACL = 0.
         RETURN
         END
