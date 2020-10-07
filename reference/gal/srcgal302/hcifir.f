      INTEGER FUNCTION HCIFIR(S)
C ----------------------------------------------------------------
C          G.Catanesi         1/9/86
C
C! Calculate the first plane crossed  by the shower
C
C        Called by CAHMOD
C -------------------------------------------------
      SAVE
      PARAMETER (LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER (LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER (LPHCT = 4)
      PARAMETER (LPHCBM = 24,LPHCES = 6)
      COMMON/HCGEGA/ HCSMTH,HCIRTH,HCLSLA,HCTUTH, NHCINL,NHCOUL,NHCTRE,
     +HCPHOF,NHCTU1(LHCNL), HCLARA(LHCNL),HCLAWI(LHCNL),IHCREG(LHCTR),
     +HCZMIN(LPHC),HCZMAX(LPHC),HCRMIN(LPHC), HCRMAX(LPHC),HCTIRF(LPHC),
     +NHCPLA(LPHC), HCWINO(LPHC),HCLTNO(LPHC)
      COMMON /HCCONG/ HCTUAC,HCSTDT,HCADCE,HADCMX,HCPFAC,
     &                HCTINS,RHBAMN,ZHECMN,ZHBAMX,HSTREA,HSTUST,
     +                NHCFSS,HCFSS1,HCFSS2,HCFLSS(100)
     &               ,HTLEMX,HCTEFF(3),HPINDU
C
      COMMON/HCLOC/HCPDIP,HCPPHI,HCPACX,HCPACY,HCPACZ ,HCPAX0,HCPAY0,
     +             HCPAZ0,FHADRC,FHCPRJ ,IHCPOR,IHCMOD,IHCIPL
      LOGICAL FHADRC,FHCPRJ
C --------------------------------------------------------------
C Starting values
      IF(HCPDIP.GT.0.)THEN
         HCIFIR= 0
      ELSE
         HCIFIR=-1
      ENDIF
C
      IF(S.LT.0.) GO TO 10
C
      HCIFIR = S/HCSMTH
C
      IF(HCPDIP.GT.0..AND.AMOD(S,HCSMTH).GT.HCTUAC) HCIFIR=HCIFIR+1
C
   10 CONTINUE
      HCIFIR =HCIFIR+1
      RETURN
      END
