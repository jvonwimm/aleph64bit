             LOGICAL FUNCTION HCDEIT(ITUB,ILAY,IMOD,IPOR)
C-----------------------------------------------------------------------
C
CKEY HCALDES HCAL STATUS TUBE / USER
C!   If .TRUE. the tube is dead
C!
C!                                     Author:G.Catanesi 01/08/89
C!
C!    INPUT:
C!           ITUB/I  :Tube #
C!           ILAY/I  :Layer #
C!           IMOD/I  :Module #
C!           IPOR/I  :Portion #
C!
C-----------------------------------------------------------------------
C
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
C! Hcal Look_up Tables of dead tubes,Tower ecc.ecc.
      COMMON/HCDETU/ MH8FDB(LPHCBM,LHCNL),MH8FDE(LPHCES*2,LHCNL-1,2)
     &              ,LSH8DB(LPHCBM,LHCNL),LSH8DE(LPHCES*2,LHCNL-1,2)
C
C
               HCDEIT = .FALSE.
C
               IF(IPOR.EQ.LPBAR)THEN
C
C    Barrel Case
                  IF(MH8FDB(IMOD,ILAY).EQ.0)RETURN
C
C   Find the eightfold #
                  CALL HNEIGH(ITUB,ILAY,IPOR,IHEIF,IDHEI)
C   Check the corresponding bit in the look_up table
                  IF(IHEIF.NE.0)THEN
                     HCDEIT = BTEST(MH8FDB(IMOD,ILAY),IHEIF)
                  ENDIF
               ELSE
C
C   EndCap Case
                  IF(IPOR.EQ.LPECB)IMOD = IMOD +LPHCES
C   Find the eightfold #
                  CALL HNEIGH(ITUB,ILAY,IPOR,IHEIF,IDHEI)
                  IF(IHEIF.NE.0)THEN
                     IDTU = IHEIF/IDHEI
                     IF(MH8FDE(IMOD,ILAY,IDTU).EQ.0)RETURN
C
C   Check the corresponding bit in the look_up table
                     HCDEIT = BTEST(MH8FDE(IMOD,ILAY,IDTU),IDHEI)
                  ENDIF
               ENDIF
C
               RETURN
               END
