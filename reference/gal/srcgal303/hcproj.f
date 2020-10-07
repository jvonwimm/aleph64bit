      SUBROUTINE HCPROJ(POS,IS,IGO,KLEAV)
C---------------------------------------------------
C
C! Project the spots given onto the nearest sensible plane
C!
C!       Author    :F.Romano    86/06/06
C!       Modified  :G.Catanesi  86/11/01
C
C      Project the spots given by Hcpars onto the
C      nearest sensible plane. If the plane is in
C      another module the projection is performed
C      Care is taken when the shower propagates at
C      a small angle respect to the sensible planes
C      without changing the reference system.
C!
C!       input :
C!            - POS/R  : coordinates in the 'SHOWER' frame
C!            - ISS/I  : first plane crossed by the shower
C!            - IGO/I  : flag (+1=Z>0 -1=Z<0)
C!
C!       output :
C!             -POS/R  : coordinates projected on nearest
C!                       sensible plane
C!             -KLEAV/I: flags the type of spot
C!                0   spot is in the same barrel module where
C!                    the shower started
C!                1   spot is in a contiguous barrel module
C!                2   spot is in the endcap
C!
C!      Called by : HCSHOW
C!      Calls     : none
C ---------------------------------------------------------
      SAVE
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
C
      COMMON /HCCONG/ HCTUAC,HCSTDT,HCADCE,HADCMX,HCPFAC,
     &                HCTINS,RHBAMN,ZHECMN,ZHBAMX,HSTREA,HSTUST,
     +                NHCFSS,HCFSS1,HCFSS2,HCFLSS(100)
     &               ,HTLEMX,HCTEFF(3),HPINDU
C
      PARAMETER (LPHC=3,LSHC=3,LPECA=1,LPECB=3,LPBAR=2)
      PARAMETER (LHCNL=23,LHCSP=2,LHCTR=62,LHCRE=3)
      PARAMETER (LHCNO = 3)
      PARAMETER (LPHCT = 4)
      PARAMETER (LPHCBM = 24,LPHCES = 6)
      COMMON/HCGEGA/ HCSMTH,HCIRTH,HCLSLA,HCTUTH, NHCINL,NHCOUL,NHCTRE,
     +HCPHOF,NHCTU1(LHCNL), HCLARA(LHCNL),HCLAWI(LHCNL),IHCREG(LHCTR),
     +HCZMIN(LPHC),HCZMAX(LPHC),HCRMIN(LPHC), HCRMAX(LPHC),HCTIRF(LPHC),
     +NHCPLA(LPHC), HCWINO(LPHC),HCLTNO(LPHC)
      COMMON/HCLOC/HCPDIP,HCPPHI,HCPACX,HCPACY,HCPACZ ,HCPAX0,HCPAY0,
     +             HCPAZ0,FHADRC,FHCPRJ ,IHCPOR,IHCMOD,IHCIPL
      LOGICAL FHADRC,FHCPRJ
C
      DIMENSION POS(*)
C
C.....Initialize flags
C
      FHCPRJ=.FALSE.
      KLEAV=0
C
C....Determine position of sensible plane along z-axis
C
      ZSENS = HCTIRF(IHCPOR)+(IS-1)*HCSMTH+ .5*HCTUAC+HCTINS

C
      IF(ABS(SIN(HCPDIP)).LT..1) THEN
C
C....Shower axis is almost parallel to sensitive planes
C
         IF((IHCPOR.EQ.LPBAR).AND.(ABS(POS(2)).GT. (POS(3)+RHBAMN
     +   )*TAN(PIBY12))) GOTO 10
C
         IF(IGO.EQ.1) THEN
            ZUPP=ZSENS-.5*HCTUAC
            ZLOW=ZSENS-HCSMTH+.5*HCTUAC
         ELSE
            ZUPP=ZSENS+HCSMTH-.5*HCTUAC
            ZLOW=ZSENS+.5*HCTUAC
         ENDIF
C
         IF((IS-IGO).LT.1) ZLOW=-9999999.
         IF((IS-IGO).GT.NHCPLA(IHCPOR))ZUPP= 9999999.
C
         IF(POS(3).GT.ZUPP) THEN
            ZSENS=ZUPP+.5*HCTUAC
            POS(3)=ZSENS+HCPACZ*HCPFAC
            GO TO 10
         ENDIF
C
         IF(POS(3).LT.ZLOW) THEN
            ZSENS=ZLOW-.5*HCTUAC
            POS(3)=ZSENS-HCPACZ*HCPFAC
            GO TO 10
         ENDIF
         GO TO 20
      ENDIF
   10 CONTINUE
C....
C   Projection with the sensible plane is given by XN,YN,ZN
C....
      WL=(ZSENS-POS(3))/HCPACZ
      XN=POS(1)+WL*HCPACX
      YN=POS(2)+WL*HCPACY
      ZN=ZSENS
C
      IF(IHCPOR.EQ.LPBAR)THEN
C
C..starting point in barrel
C
         IF(ABS(XN).LE.ZHBAMX) THEN
C..barrel ==> barrel
            YSENS=(ZN+RHBAMN)*TAN(PIBY12)
C
Check if the projection is inside the same module
C
            IF(ABS(YN).GT.YSENS)THEN
C
C.....it is in the nearby module
C
               WLZY=SQRT((ZN-POS(3))**2+(YN-POS(2))**2)
               ALPHA=ATG(HCPACZ,HCPACY)
               IF(ALPHA.GT.PIBY2)ALPHA=PI-ALPHA
               DY = ABS(YN) - YSENS
               DLZY=SIN(PIBY6)*DY/SIN(PI-(ALPHA+PIBY6))
               DLZY=SIGN(DLZY,ZN-POS(3))
               SLZY=WLZY-DLZY
C
               POS(1)=(DLZY*POS(1)+SLZY*XN)/WLZY
               POS(2)=(DLZY*POS(2)+SLZY*YN)/WLZY
               POS(3)=(DLZY*POS(3)+SLZY*ZN)/WLZY
               KLEAV=1
            ELSE
C
               POS(1) = XN
               POS(2) = YN
               POS(3) = ZN
            ENDIF
C
         ELSE
C.
C.....barrel ==> endcap
C.
            APOS=ABS(POS(1))-ZHBAMX-HCTIRF(LPECA)-HCTUAC
C
            IF(APOS.LE.0.) THEN
               ISECAP=1
            ELSE
               ISECAP=INT(APOS/HCSMTH)+1
            ENDIF
            IF(ISECAP.GT.NHCPLA(LPECA)) GO TO 20
C
            XSENS=ZHBAMX+HCTIRF(LPECA)+(ISECAP-1)*HCSMTH +.5
     &      *HCTUAC+HCTINS
            XSENS=SIGN(XSENS,XN)
            WL=(XSENS-POS(1))/HCPACX
            POS(1)=XSENS
            POS(2)=POS(2)+WL*HCPACY
            POS(3)=POS(3)+WL*HCPACZ
            KLEAV=2
         ENDIF
C
      ELSE
C..end cap
C.....endcap ==> barrel
C         (to be implemented)
C....
C    Store projection coordinates for further analysis
C
         POS(1)=XN
         POS(2)=YN
         POS(3)=ZN
         KLEAV = 2
C
      ENDIF
C......projection found......
      FHCPRJ=.TRUE.
C
      IF(FHCDB2)THEN
         WRITE(LOUTIO,510)(POS(J),J=1,3),IGO
      ENDIF
      RETURN
C................................
   20 CONTINUE
      IF(FHCDB2)THEN
         WRITE(LOUTIO,500)
         WRITE(LOUTIO,510)(POS(J),J=1,3),IGO
      ENDIF
      RETURN
  500 FORMAT(1X,'++++HCPROJ++++',5X,'projection not found')
  510 FORMAT (1X,'+++HCPROJ+++ spot position ',3F10.3,I4)
      END
