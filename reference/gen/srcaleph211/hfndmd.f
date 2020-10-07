         SUBROUTINE HFNDMD(INTET,INPHI,NSTAK,ISUBC,MODUL,
     *   IOVRL)
C-----------------------------------------------------------------
CKEY HCALDES HCAL TOWER MODULE  / USER
C! get module #, subcomp # from I,J,K for Hcal
C!
C!      G.Capon               date : 861001
C! input :
C!         INTET : theta index of storey
C!         INPHI : phi     "        "
C!         NSTAK : stack number
C! output :
C!        ISUBC  : subcomp # (1=endcapA,2=barrel,3=endcapB)
C!        MODUL  : phys module # (1-6 for endcaps,1-24 for barrel)
C!        IOVRL  : position flag (if ne 0 storey is splitted between
C!                       endcap and barrel jonction region)
C!
C-----------------------------------------------------------------
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
         NREG=IHCREG(INTET)
         ISUBC=2
         IF(INTET.LE.13) ISUBC=1
         IF(INTET.GE.50) ISUBC=3
C
         IF (ISUBC.EQ.2) THEN
              MODUL=(INPHI-1)/4+1
              ELSE
              NTSEX=2**(NREG+1)
              MODUL=(INPHI-1)/NTSEX+1
              ENDIF
C
C?        flag stories in barrel/endcap overlap region
C
          IOVRL=0
          IF(INTET.GE.14.AND.INTET.LE.16) IOVRL=1
          IF(INTET.LE.49.AND.INTET.GE.47) IOVRL=1
C
          RETURN
          END
