      SUBROUTINE HCCOIN
C ---------------------------------------------------------
C
C! Compute intersection with Tubes for the Full Generator Mode level
C!
C!     Author :G.Catanesi 87/01/30
C!     Mod. by G.Catanesi 89/04/13 to take in account
C!                                    tube's inefficency
C!            Mod. by G.Catanesi 89/08/01
C!                    to take in account dead zone and dead tubes
C           Mod by L. Silvestris 18/3/92
C!
C!         Input  : McTrackElement stored in common TRKCOM
C!
C!         Output : HCSE  McHcTubesSegment
C!
C!
C!   -Called by :HCHIT
C!       -Calls : HCFITU, HCSTSE, HTDEAD from this .HLB
C!                GTRNSF from CERNLIB
C!                HCMDPL, HCDEIT         from Alephlib
C ------------------------------------------
      SAVE
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
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
      COMMON/HCLOC/HCPDIP,HCPPHI,HCPACX,HCPACY,HCPACZ ,HCPAX0,HCPAY0,
     +             HCPAZ0,FHADRC,FHCPRJ ,IHCPOR,IHCMOD,IHCIPL
      LOGICAL FHADRC,FHCPRJ
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
      PARAMETER (LTUB=100)
      INTEGER ITUBE(LTUB)
      LOGICAL HCDEIT,LDEAD
      INTEGER HCEFF
      REAL XTUB(LTUB),YTUB(LTUB),DXTUB(LTUB),YPART(LTUB)
      DIMENSION VIN(3),VOUT(3),WIN(3),WOUT(3),COORI(2),COORO(2)
C ----------------------------------------------------------------
      IF (ITRKEL(8).EQ.1) THEN
         IF (TRKVOL.EQ.'HBL1' .OR. TRKVOL.EQ.'HBL2') THEN
            CALL GETRAN (TRKELE(15))
         ENDIF
         VIN(1) = TRKELE(1)
         VIN(2) = TRKELE(2)
         VIN(3) = TRKELE(3)
         CALL GTRNSF (TRKELE(1),TRKELE(15),TRKELE(18),WIN(1))
C
      ELSE IF((ITRKEL(8).EQ.2) .OR. (ITRKEL(9).NE.0)) THEN
C
         IHCIPL = ITRKEL(6)
         IMOD = ITRKEL(10)
         IHCMOD=IMOD
         CALL GTRNSF (TRKNXT(1),TRKELE(15),TRKELE(18),WOUT(1))
C
         CALL HCMDPL(IHCPOR,IMOD,IHCIPL,WIN,COORI)
         CALL HCMDPL(IHCPOR,IMOD,IHCIPL,WOUT,COORO)
C
C      Find hit tubes
C
         XX = (COORI(2)+COORO(2))/2.
         DX = ABS(COORI(2)-COORO(2))
         YY = ABS(COORI(1)+COORO(1))/2.
         DY = ABS(COORI(1)-COORO(1))
C
         IF(IHCPOR.EQ.2)THEN
            COSXZ = TRKELE(6)
         ELSE
            COSXZ = TRKELE(4)
         ENDIF
C
         IF(FHCDB2)THEN
            WRITE(LOUTIO,500)ITRKEL(1),IHCIPL
         ENDIF
C
C evaluates the projection on the wire DX
C
         DX = DX * HSTUST
         DY = DY * HSTUST
         XL = VDIST(WIN,WOUT,3) * HSTUST
C
C  cuts very long track elements
C
         IF(XL.GT.HTLEMX)THEN
            DX = DX * (HTLEMX/XL)
            DY = DY * (HTLEMX/XL)
         ENDIF
C
C - get a list of hit tubes
C
         CALL HCFITU(IHCPOR,IHCIPL,YY,DY,ITUBF,NTUB,YPART,NTUSP,WSPAC)
         IF(FHCDB2)THEN
            WRITE(LOUTIO,510)YY,DY,ITUBF,NTUB
         ENDIF
         IF(NTUB.LE.0) GOTO 20
C
         DO 10 I=1,NTUB
C
            ITUBE(I) = ITUBF+I-1
            DXTUB(I) = DX*YPART(I)
            IF(I.EQ.1)THEN
C           1st tube of the list
               YTUB(I) = YY + (YPART(I)-1.)*DY/2.
               XTUB(I) = XX + SIGN ((YPART(I)-1.)*DX/2.,COSXZ)
            ELSEIF (I.NE.NTUSP) THEN
C           next tube of the list
               YTUB(I) = YTUB(I-1)+ (YPART(I-1)+YPART(I))*DY/2.
               XTUB(I) = XTUB(I-1)+
     &                      SIGN ((DXTUB(I-1)+DXTUB(I))/2.,COSXZ)
            ELSE
C           tube which is replaced by a spacer
               XTUB(I) = XTUB(I) + SIGN (WSPAC*ATAN2(DX,DY),COSXZ)
               YTUB(I) = YTUB(I) + WSPAC
            ENDIF
            DXTUB(I)=MAX (HCSTDT,DXTUB(I))
C
   10    CONTINUE
C
C - if required kill not working tubes
C
          IF(ICHCJO(6).EQ.0)THEN
            DO 60 J=1,NTUB
               IF(ITUBE(J).GT.0)THEN
                  LDEAD = HCDEIT(ITUBE(J),IHCIPL,IHCMOD,IHCPOR)
                  IF(LDEAD) ITUBE(J) = -ITUBE(J)
               ENDIF
 60         CONTINUE
         ENDIF
C
C - Kill tubes for inefficency if required (==> tube# < 0)
C
         IF(ICHCJO(5).EQ.0)THEN
            DO 30 I=1,NTUB
              IF (ITUBE(I).GT.0) THEN
                IF (RNDM(DUM).GE.HCTEFF(IHCPOR)) ITUBE(I) = -ITUBE(I)
              ENDIF
 30         CONTINUE
         ENDIF
C
C - Take into account dead zones inside tubes
C
         IF(ICHCJO(3).EQ.0)THEN
            DO 40 J=1,NTUB
               IF(ITUBE(J).GT.0)THEN
                  CALL HTDEAD(XTUB,DXTUB,ITUBE(J),IDEAD)
                  IF(IDEAD.EQ.1) ITUBE(J) = -ITUBE(J)
               ENDIF
 40         CONTINUE
         ENDIF
C
C - Store fired tubes in the working bank JDHCSE
C
         CALL HCSTSE (VIN,IHCPOR,IMOD,IHCIPL,YTUB,XTUB,DXTUB,ITUBE,NTUB)
C
      ENDIF
C
   20 CONTINUE
      RETURN
  500 FORMAT(1X,' +++HCCOIN+++ Track #',I6,' Plane #',I3)
  510 FORMAT(1X,' +++HCCOIN+++ YY  , dy ',2F12.4,3X,'1st tube',I4,3X,
     & '# of tubes',I3)
      END
