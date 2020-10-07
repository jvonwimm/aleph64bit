      SUBROUTINE ECHIT
C -------------------------------------------------------------
C - J.Badier  F.Ranjard - 870817
C! ECAL analog signal steering
C - Initialize BOS banks at beginning of event
C - Get sub-component # and module #
C - IF it is a geantino particle THEN treat it and RETURN
C   ELSE treat the McTreackElement stored in /TRKCOM/
C - IF shower parametrization is required AND it is
C         a photon which stops loosing all its energy
C      OR a hadron which stops because of an hadronic
C         mechanism
C   THEN treat the shower parametrization depending of the
C      type of parametrization chosen.
C - called by   GUSTEP                     from GEANT3 lib
C - calls       EHSHOW, EHTRKE, EHCUTF
C               EHCUTG, CATINO             from this .HLB
C               ELIMSC                     from ALEPHLIB
C --------------------------------------------------
      SAVE
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
      EQUIVALENCE (MODUL,ITRKEL(10))
      LOGICAL FNOPA
C
      DATA NCALL /0/
C
C - geantino particle
      FTINO = ITRKEL(11).EQ.6
C - muon particle
      FMUON = ITRKEL(11).EQ.5
C - e+/e- particle
      FELEC = ITRKEL(11).EQ.2
C - charged hadron
      FHADC = ITRKEL(11).EQ.4
C
C --------------------------------------------------------------
C - NO parametrizatio required
      FNOPA = ICECJO(5).EQ.0
C --------------------------------------------------------------
C
C - Initialize BOS banks at beginning of event
C
      IF (FBEGJO(4)) THEN
          IF (NCALL.EQ.0) THEN
             NCALL = 1
             CALL ELIMSC(ZECA,ZECB)
           ENDIF
           FBEGJO(4) = .FALSE.
           CALL EHIBOS
        ENDIF
C
C - Find sub-component : 1 end cap z>0, 2 barril, 3 end cap z<0
C   the module # is stored in ITRKEL(10)
C
      ISCMP = 1
      IF (TRKELE(3).LT.ZECA) ISCMP = 2
      IF (TRKELE(3).LT.ZECB) ISCMP = 3
C
C -------------------------------------------------------------
C
C - IF it is a geantino THEN
C      IF it is the 1st entry (no shower param. defined) THEN
C         create parameters, compute XYZ, deposit E
C      THEN RETURN
C
      IF (FTINO) THEN
         IF ( ITRKEL(8).GE.2) CALL EHSHOW(ISCMP,MODUL)
         GOTO 999
C - ELSE it is not a geantino : treat the track element
C
      ELSE
         IF (ITRKEL(8).NE.1) CALL EHTRKE (ISCMP,MODUL)
      ENDIF
C
C -------------------------------------------------------------
C - IF NO shower parametrization is required THEN RETURN
      IF (FNOPA) GOTO 999
C
C - IF it is a charged hadron which does not make an had.interaction
C   THEN RETURN
      IF (FHADC .AND. .NOT.FTRHAD) GOTO 999
C
C - IF it is a muon THEN RETURN
      IF (FMUON) GOTO 999
C --------------------------------------------------------------
C
C - shower parametrization is considered
C
C - IF it is an e+/e- THEN decide wheither Yes/No a shower can
C      be developped , IF Yes THEN develop the shower, stop the
C      current particle and RETURN
C
      IF (FELEC) THEN
         IF(ITRKEL(8).EQ.1)  THEN
           DO 1 I=1,LNTRK
             TRKNXT(I) = TRKELE(I)
    1      CONTINUE
         ENDIF
         CALL EHCUTF (MFLAG)
         IF (MFLAG.EQ.1) THEN
            CALL EHSHOW (ISCMP,MODUL)
            ITRKEL(9) = NOMOR
            GOTO 999
         ENDIF
      ENDIF
C
C - last case it is an e+/e- with a shower not fully contained (MFLAF=0)
C    OR a photon OR a hadron : the action depends on the type of
C   parametrization.
C   IF geantino parametrization (FPARJO is true) THEN try to create
C      a geantino particle.
C
      IF (FPARJO) THEN
         CALL EHCUTG (MFLAG)
         IF (MFLAG.EQ.1) THEN
             CALL CATINO
             ITRKEL(9) = NOMOR
          ENDIF
      ENDIF
C
 999  CONTINUE
C
C    length vs energy for the track element of an e+ or e-
C                    or a photon
C
       IF(FHISJO(4)) THEN
           WSKI=1.
           IDSKI=439
           IF(ITRKEL(4).EQ.1) THEN
               CALL HFILL(IDSKI,TRKELE(12),TRKELE(11),WSKI)
           ELSE IF(ITRKEL(4).EQ.2) THEN
               CALL HFILL(IDSKI+1,TRKELE(12),TRKELE(11),WSKI)
           ENDIF
       ENDIF
C
      END
