      SUBROUTINE HCHIT
C --------------------------------------------
C
C! Main routine to process each track element
C!
C!       Author   :G.Catanesi      86/11/24
C!       Mod.     :G.Catanesi      87/11/19
C!       Mod.     :L.Silvestris    18/3/93
C!       input:  McTrackElement stored in common TRKCOM
C!       output bank: HCSE McHcTubeSegments
C!
C!
C!   -Called by : GUSTEP
C!   -Calls     : HCCOIN (detailed geometry)  from this .HLB
C!                HCSHOW (geantino electron param) from this .HLB
C!                HCIEVE, HNEWTK (general) from this .HLB
C!
C -------------------------------------------
      SAVE
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
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
      PARAMETER (LGKINE=100)
      COMMON/GCKING/KGCASE,NGKINE,GKIN(5,LGKINE),GTOFD(LGKINE)
     &             ,IGFLGK(LGKINE)
C
      COMMON/GCKIN3/GPOS(3,LGKINE)
      REAL GPOS
C
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
C
      COMMON/HCLOC/HCPDIP,HCPPHI,HCPACX,HCPACY,HCPACZ ,HCPAX0,HCPAY0,
     +             HCPAZ0,FHADRC,FHCPRJ ,IHCPOR,IHCMOD,IHCIPL
      LOGICAL FHADRC,FHCPRJ
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
      LOGICAL FCHRG,FSENS
C - geantino particle
      FTINO = ITRKEL(11).EQ.6
C - muon particle
      FMUON = ITRKEL(11).EQ.5
C - e+/e- particle
      FELEC = ITRKEL(11).EQ.2
C - charged hadron
      FHADC = ITRKEL(11).EQ.4
C
C ----------------------------------------------------
C
      FCHRG = TRKELE(14) .NE. 0.
      FSENS = ITRKEL(7).GT.0
C-----------------------------------------------------
C?   evaluate the portion number
C
      IF (TRKVOL(2:2).EQ.'B') THEN
         IHCPOR = 2
      ELSE IF (TRKELE(3).GT.0.) THEN
         IHCPOR = 1
      ELSE
         IHCPOR = 3
      ENDIF
C
C?  initialize same variables at the beginning
C?          of the event
      IF(FBEGJO(7)) THEN
         CALL HCIEVE
      ENDIF
C
      IF(FHCDB2) CALL HNEWTK
C
C============ detailed geometry used in full generator and geantino
C
C - IF geantino particle THEN
C      deposit energy following /CAPANO/ parameters and RETURN
C   ELSE it is a track element
C      IF charged particle in a sensitive volume THEN
C         treat the track element: deposit energy
C      ENDIF
C   ENDIF
C   IF muon THEN RETURN
C   IF charged had. which does not stop with had. mechanism RETURN
C   ENDIF
C
      IF (FTINO) THEN
         CALL HCSHOW
         GOTO 10
      ELSE
         IF (FSENS .AND. FCHRG) THEN
            IF((ITRKEL(8).EQ.0) .AND. (ITRKEL(9).EQ.0))RETURN
            NHCC01 = NHCC01 + 1
            CALL HCCOIN
         ENDIF
      ENDIF
      IF (FMUON) GOTO 10
      IF (FHADC .AND. .NOT.FTRHAD) GOTO 10
C
   10 CONTINUE
      FBEGJO(7) = .FALSE.
C
      RETURN
      END
