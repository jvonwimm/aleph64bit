       SUBROUTINE HFENHI(ISUBC,MODAQ,LAYER,LWIRE,NWCLU,ISEXT,XHIT,
     * CLUW,NHIT,NNWIRE,NHITWI)
C-------------------------------------------------------------------
CKEY HCALDES HCAL ENDCAP WIRE CLUSTER HIT / USER
C! converts wire clusters to hits in Endcap
C!
C!      G.Capon    17-feb-1992 (extended version of old HFEHIT)
C!                 mods -  900120 take air between tubes
C!                         into account : enlarge iron edge and spacers
C!      F.Ranjard  mods -  900214 remove the previous mod.
C!                         because the iron edge and spacer width are
C!                         changed in HRDDAF
C!      1 cluster ==> 1 hit, except if dead zones or spacers
C!      fall inside the cluster
C! input :
C!      ISUBC : subcomponent number
C!      MODAQ : acquisition processor number (1-12)
C!      LAYER : streamer tubes layer number
C!      LWIRE : last wire(strip) of a cluster of consecutive hit wires
C!      NWCLU : number of consecutive hit wires in a cluster (1-16)
C! output:
C!    ISEXT   : physical module # (sextant #)
C!    XHIT(N) : local coordinate for hit N
C!    CLUW(N) : cluster width for hit N
C!    NHIT    : number of hits for this cluster (=1 except if dead zones
C!   NNWIRE(I): wire number of wire at position I inside cluster
C!   NHITWI(I): hit    "                "            "
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
        INTEGER NNWIRE(*),NHITWI(*)
C
        NHIT=0
        IF(LWIRE.GE.NHCTRE)                                     GO TO 40
C
        NFWIR=LWIRE-NWCLU+1
C
        DO 10 J=1,3
             CLUW(J) = 0.
   10   CONTINUE
C
C?              get sextant # ISEXT (1,6) and wire offset (0 or 160)
C
        IOFFS=0
        NTBOF=2*NHCEIT*NHCIND
C
        IF(ISUBC.EQ.1)THEN
        ISEXT=(MODAQ+1)/2
        IF(ISEXT.EQ.3.OR.ISEXT.EQ.6) THEN
                IF(MOD(MODAQ,2).EQ.1)IOFFS=NTBOF
                ELSE
                IF(MOD(MODAQ,2).EQ.0)IOFFS=NTBOF
                ENDIF
        ELSE
        ISEXT=(MODAQ-23)/2
        IF(ISEXT.EQ.1.OR.ISEXT.EQ.4) THEN
                IF(MOD(MODAQ,2).EQ.0)IOFFS=NTBOF
                ELSE
                IF(MOD(MODAQ,2).EQ.1)IOFFS=NTBOF
                ENDIF
        ENDIF
C
C?           loop over wires in cluster
C
        DO 20 ICLU=1,NWCLU
        IWIRE=NFWIR+ICLU-1+IOFFS
C
C?        generate new hit if spacer inside cluster
C
        IMOD=MOD(IWIRE,80)
        IF(IMOD.EQ.0.OR.ICLU.EQ.1) THEN
                NHIT=NHIT+1
                FNSP=IWIRE/80
                XFHIT(NHIT)=XENDC0+HCSAEC*FLOAT(IWIRE)+
     *          FNSP*HCAPSL
                ENDIF
        CLUW(NHIT)=CLUW(NHIT)+HCSAEC
        NNWIRE(ICLU)=IWIRE - IOFFS
        NHITWI(ICLU)=NHIT
   20    CONTINUE
C
C?            compute hit center and width
C
        DO 30 IHIT=1,NHIT
        XHIT(IHIT)=XFHIT(IHIT)+(CLUW(IHIT)-HCSAEC)/2.
   30   CONTINUE
C
   40    RETURN
         END
