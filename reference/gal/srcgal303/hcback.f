      SUBROUTINE HCBACK(POS,KLEAV,VIN,XX,YY)
C-----------------------------------------------------
C
C! Determine spot coordinate in the plane which containing the spot
C! Transform spot coordinates from local frame to the Aleph frame
C!
C!  Author    :F.Romano   86/7/20
C!  Modified  :G.Catanesi 86/11/1,87/10/21,88/07/06
C!
C!          INPUT:
C!               - POS/R  : spot coordinates in the local frame
C!               - KLEAV/I: flag of the type spot type [0,2]
C!
C!          OUTPUT:
C!                - VIN/R   : spot coordinates in the Aleph frame
C!                - XX/R    : spot coordinate in the tube plane
C!                                                 along wires
C!                - YY/R    : spot coordinate  in the tube plane
C!                                  normal to the wires
C!       Called by : HCSHOW
C!    Calls     :HCRINV
C!
C ---------------------------------------------
      SAVE
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
      PARAMETER  (D360=TWOPI*RADEG   ,D180=PI*RADEG   ,D90=0.5*D180)
      PARAMETER  (D45=0.5*D90   ,D15=D45/3.   ,D210=7.*D90/3.)
      PARAMETER  (D225=D180+D45   ,D270=D180+D90  ,D7P5=0.5*D15)
      PARAMETER(LSENV=30)
      PARAMETER (LIMVOL=17)
C
      COMMON/AGCONS/ IAGROT,IAGMAT,IAGMED,IAGFHB,IAGSLV,IAGSEN(LSENV,2)
     2      , NAGIMP,LAGIMP(3,LIMVOL)
C
       COMMON /WRKSPC/ WSPACE(88320)
      PARAMETER (LPTAB=50)
      DIMENSION PTAB(LPTAB),JTAB(LPTAB)
      EQUIVALENCE (PTAB(1),JTAB(1),WSPACE(1))
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
C
      DIMENSION POS(*),VIN(*),SPOT(3)
C -----------------------------------------------------------
      UNO=1.
      IF(IHCPOR.EQ.LPBAR) THEN
C.....
C    Shower started in the barrel
C.....
         IF(MOD(IHCMOD,2).EQ.1) UNO=-1.
C....Transform coordinates from parametrization frame to module frame
         SPOT(1)=UNO*POS(2)
         SPOT(2)= POS(3)+RHBAMN
         SPOT(3)=UNO*POS(1)
         NROT = IHCMOD
         IF(KLEAV.EQ.1) THEN
            IUNO=1.
            IF(POS(2).GT.0.) IUNO=-1.
C.....
C    If spot is in a contiguous module, rotate coordinates in the
C    parametrization frame before the calculation of XX and YY.
C.....
            POS(3)=SPOT(2)
            YY = POS(2)*COS(PIBY6) + IUNO*POS(3)*SIN(PIBY6)
            ZZ = -IUNO*POS(2)*SIN(PIBY6) + POS(3)*COS(PIBY6)
            POS(2) = YY
            POS(3) = ZZ - RHBAMN
            IHCMOD= IHCMOD+2*IUNO
         ELSE
C...
Check if the spot is in the endcap
C...
            IF(KLEAV.EQ.2) GO TO 10
         ENDIF
C
         XX=POS(1)
         YY=POS(2)*UNO
         IF(YY.LE.0.)IHCMOD=IHCMOD+INT(UNO)
         IF(IHCMOD.GT.LPHCBM)IHCMOD=IHCMOD-LPHCBM
         IF(IHCMOD.LT.1)IHCMOD=IHCMOD+LPHCBM
         YY = ABS(YY)
         ZZ = POS(3)
      ELSE
C.....
C    Shower started in the endcap
C.....
         NROT = LPHCBM+1
         IF(IHCPOR.EQ.LPECB) THEN
            NROT = NROT+1
            UNO = -1
         ENDIF
C....In the endcap parametrization frame and module frame are equivalent
C....Only translate z-coordinate origin
         SPOT(1)= POS(1)
         SPOT(2)= POS(2)
         SPOT(3)= POS(3)+ZHECMN
      ENDIF
C
   10 CONTINUE
C
C.....
C    Transform from the module frame to the Aleph frame
C.....
      CALL HCRINV(NROT,SPOT,VIN)
C
      IF(IHCPOR.EQ.LPBAR.AND.KLEAV.NE.2) GOTO 20
C....
Calculate module number in the endcap and x-y coordinate in the tube pla
Care is taken for modules 2 and 5 where orientation of tubes depends on
C endcap
C....
      ALPHA=ATG(VIN(2),VIN(1))
      IHCMOD = INT(ALPHA/PIBY3)+1
      UX = 1.
      UY = 1.
      IF(IHCMOD.GT.3) UY=-UY
      IF(IHCMOD.GT. 1.AND. IHCMOD.LT.5) UX=-UX
C
      IF(IHCMOD.EQ. 2.OR. IHCMOD.EQ.5) THEN
         XX = -UNO*VIN(1)*COS(PIBY3)-VIN(2)*SIN(PIBY3)
         YY = -UNO*VIN(1)*SIN(PIBY3)+VIN(2)*COS(PIBY3)
      ELSE
         XX = VIN(1)
         YY = VIN(2)
      ENDIF
C
      XX = UX*XX
      YY = UY*YY
C
      IF(IHCPOR.NE.LPBAR) GOTO 20
      IF(VIN(3).GT.0.) THEN
         IHCPOR = LPECA
      ELSE
         IHCPOR = LPECB
      ENDIF
C
   20 CONTINUE
C....
C    Fill ILAY with the tube plane number
C....
      ZZ = POS(3) - HCTIRF(IHCPOR)
      IHCIPL = INT(ZZ/HCSMTH)+1
C
   30 CONTINUE
C
      RETURN
      END
