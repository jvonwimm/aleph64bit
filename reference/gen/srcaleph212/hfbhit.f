       SUBROUTINE HFBHIT(ISUBC,MODAQ,LAYER,LWIRE,NWCLU,MODBA,XHIT,
     * CLUW,NHIT)
C-------------------------------------------------------------------
CKEY HCALDES HCAL BARREL WIRE CLUSTER HIT / USER
C! converts wire clusters to hits in Hbarrel
C!
C!      G.Capon               date : 861001
C!     1 cluster ==> 1 hit, except if dead zones or spacers
C!      fall inside the cluster
C! input :
C!      ISUBC : subcomponent number
C!      MODAQ : acquisition processor number (1-12)
C!      LAYER : streamer tubes layer number
C!      LWIRE : last wire(strip) of a cluster of consecutive hit wires
C!      NWCLU : number of consecutive hit wires in a cluster (1-16)
C! output:
C!   MODBA(N) : phys module # for hit N (a cluster may be on 2 modules)
C!    XHIT(N) : local coordinate for hit N
C!    CLUW(N) : cluster width for hit N
C!    NHIT    : number of hits for this cluster (=1 except if dead zones
C!
C--------------------------------------------------------------------
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
        REAL XHIT(*),CLUW(*),XFHIT(3)
        INTEGER MODBA(*)
        DIMENSION IFWAS(2)
C
C?      NEITSP =nb 8tubes before spacer1
C?      HCSPLT =width of spacer 1
C?      XBARR0 = coordinate of wire 0
C?      IFWAS(I) = Indes of First Wire After Spacer I
C
        NWMOD=NHCEIT*NEITHC(LAYER)
        NWMAX=2*NWMOD-1
        NHIT=0
        IF(LWIRE.GT.NWMAX)                                      GO TO 50
        DO 10 ISP=1,2
   10   IFWAS(ISP)=NHCEIT*NEITSP(LAYER,ISP)
        NFWIR=LWIRE-NWCLU+1
C
        DO 20 J=1,3
           CLUW(J) = 0.
   20   CONTINUE
C
C?           loop over wires in cluster
C
        DO 30 ICLU=1,NWCLU
        NWIRE=NFWIR+ICLU-1
        IWIRE=NWIRE
        IF(NWIRE.GE.NWMOD) IWIRE=NWMAX-NWIRE
C?             if first wire of cluster generate new hit
C?            if wire after modules jonction generate new hit
C?             if wire after spacer generate new hit
        IF (ICLU.EQ.1  .OR.  NWIRE.EQ.NWMOD  .OR.  IFWAS(1).EQ.IWIRE
     +  .OR.  IFWAS(2).EQ.IWIRE) THEN
           NHIT=NHIT+1
           XFHIT(NHIT)=XBARR0+HCSABL*FLOAT(IWIRE)
           IF(IWIRE.GE.IFWAS(1))XFHIT(NHIT)=XFHIT(NHIT)+HCSPLT(LAYER,1)
           IF(IWIRE.GE.IFWAS(2))XFHIT(NHIT)=XFHIT(NHIT)+HCSPLT(LAYER,2)
           MODBA(NHIT)=2*(MODAQ-12)-1+NWIRE/NWMOD
           ENDIF
        CLUW(NHIT)=CLUW(NHIT)+HCSABL
   30   CONTINUE
C
C?            compute hit center and width
C
        DO 40 IHIT=1,NHIT
        XHIT(IHIT)=XFHIT(IHIT)+(CLUW(IHIT)-HCSABL)/2.
   40   CONTINUE
C
   50   RETURN
        END
