      SUBROUTINE HCSHOW
C------------------------------------------------------------
C
C! Parametrize showers (Geantino mode)
C!
C!           Authors     :G.Catanesi 87/12/14
C!           Mod         :G.Catanesi 89/08/01
C!           Modified by :L.Silvestris 18/3/93
C!
C!       Called by :HCHIT                         from this .HLB
C!       Calls to :CAHMOD, CAHERB, HCPROJ, HCBACK from this .HLB
C!                 HCFITU,HCSTEN,HCSTSE
C!                 HCDEIT,HTDEAD                   from Alephlib
C!
C ----------------------------------------
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
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
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
C
      COMMON/HCLOC/HCPDIP,HCPPHI,HCPACX,HCPACY,HCPACZ ,HCPAX0,HCPAY0,
     +             HCPAZ0,FHADRC,FHCPRJ ,IHCPOR,IHCMOD,IHCIPL
      LOGICAL FHADRC,FHCPRJ
      PARAMETER(LTYPEL=48, LTYPME=49, LTYPBA=50, LTYPAN=51, LTYPGA=52)
      COMMON / CAPANO / NATGER,METHCO,TOTNRJ,TINOX0,TINOL0,EMGNRJ,
     + EMALFA,EMBETA,EMALM1,EMAEXP,EMFACT,EMUDP0,EMUDP2, HADNRJ,HALPHA,
     + HABETA,HADRAY,HAPUIS,HGAMMA,SMAXLR,SMAXLA
       COMMON / CANAME / EMNAME
       CHARACTER*4 EMNAME
C
C - user stop particle flag
      PARAMETER (NOMOR=3)
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      LOGICAL FTINO, FMUON, FELEC, FHADC
      REAL POS(3),VIN(3)
      LOGICAL HCDEIT,LDEAD
C =====================================================
C - geantino particle
      FTINO = ITRKEL(11).EQ.6
C - muon particle
      FMUON = ITRKEL(11).EQ.5
C - e+/e- particle
      FELEC = ITRKEL(11).EQ.2
C - charged hadron
      FHADC = ITRKEL(11).EQ.4
C
C ------------------------------------------------------------
C?    Here it is a shower
      IF (FHCDB2)THEN
         WRITE (LOUTIO,'(/1X,''+++HCSHOW++ ITRKEL '',4I5,1X,A4,
     &                   6I5,L4)')
     &     (ITRKEL(I),I=1,4),TRKVOL,(ITRKEL(I),I=6,11),FTRHAD
         WRITE (LOUTIO,'(1X,''TRKELE '',10F12.4)') TRKELE
         WRITE (LOUTIO,'(1X,''TRKNXT '',10F12.4)') TRKNXT
      ENDIF
C
C - fill bank HCVL,NR=track# and common /CAHTRA/
C   get the plane# IHPLN where interaction occurs
C - initialization ihpln=99
      IHPLN = 99
      CALL CAHMOD (IPAR,IHPLN)
C
C - IF initialization (IPAR=0) THEN  RETURN
      IF (IPAR . NE. 1) GO TO 30
C
      IEMG = 0
      CALL VZERO(POS,3)
      CALL CAHERB(EMGNRJ,IEMG,POS,NBEM)
      IF (IEMG.LT.1) GO TO 20
      ISS = IHPLN
      IF(ISS.LT.1.OR.(ISS.GT.NHCPLA(IHCPOR))) GO TO 20
      IPOR0 = IHCPOR
      IGO = SIGN(1.,HCPDIP)
      HCENMN = 0.
      ILEAV = 999
C
      IF (IEMG .EQ. 0) GO TO 20
C
      NEM = 0
C   LOOP ON E.M. SHOWER IN HCAL
   10 CONTINUE
      NBEM = 0
      IF (IEMG.EQ.0) GO TO 20
      CALL VZERO(POS,3)
      CALL CAHERB(EMGNRJ,IEMG,POS,NBEM)
      IF (NBEM .GT.0) NEM = NEM+1
      IF(NBEM.EQ.0) GO TO 10
C
C?....Search projection on sensible plane
C
      CALL HCPROJ(POS,ISS,IGO,KLEAV)
C..
C?....Transform back to the Aleph reference system
C..
      IF (FHCPRJ) THEN
         CALL HCBACK(POS,KLEAV,VIN,XX,YY)
C?        find tube number
         YY = ABS(YY)
         DY = 0.
         CALL HCFITU(IHCPOR,IHCIPL,YY,DY,ITUBF,NTUB,YPART,NTUSP,WSPAC)
C
C?        Store information in HCSE bank
         IF (NTUB.GT.0) THEN
            IMOD = IHCMOD
            IHCIPL = IHPLN
C
C          if required kill not working tubes
            IF(ICHCJO(6).EQ.0)THEN
               LDEAD = HCDEIT(ITUBF,IHCIPL,IHCMOD,IHCPOR)
               IF(LDEAD)GOTO 40
            ENDIF
C
C - Kill tubes for inefficency if required
C
            IF(ICHCJO(5).EQ.0)THEN
               IF(RNDM(DUM).GE.HCTEFF(IHCPOR)) ITUBF = -ITUBF
               IF(ITUBF.LT.0) GOTO 40
            ENDIF
C
C          Take in account dead zones inside tubes
            IF(ICHCJO(3).EQ.0)THEN
               DXX = 0.
               CALL HTDEAD(XX,DXX,ITUBF,IDEAD)
               IF(IDEAD.EQ.1)GOTO 40
            ENDIF
C
C?          Take in account single streamer energy deposit fluctuation
            HCENOR = HCSTEN(1)
            CALL HCSTSE(VIN,IHCPOR,IMOD,IHCIPL,YY,XX,HCENOR,ITUBF,1)
         ENDIF
 40      CONTINUE
      ENDIF
C?....End of loop
C
      IHCPOR = IPOR0
C.................................................
C?Check if shower enters a new module to change step
C
      IF (IEMG.NE.0) GO TO 10
C
   20 CONTINUE
C
      IF (IHPLN.EQ.NHCPLA(IHCPOR)) THEN
C-    STOP GEANTINO TRACKING
         ITRKEL (9) = NOMOR
      ENDIF
   30 CONTINUE
C
      END
