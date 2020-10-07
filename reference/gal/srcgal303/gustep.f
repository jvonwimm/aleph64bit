      SUBROUTINE GUSTEP
C ----------------------------------------------------------------------
C. - F.Ranjard - 850418
C. - GEANT3 user routine called at the end of every step in GTVOL
C.   and also when entering a new volume in GTRACK.
C.   At this level a track element is avalaible in the GEANT common
C.   block /GCTRAK/, /GCTMED/, /GCKINE/. Secondaries are
C.   stored in /GCKING/.
C.   If the track element is in a selected sensitive volume
C.   (IGSVOL>0 and SETS data card) the common /TRKCOM/ is
C.   filled in and control is transferred  to the corresponding
C.   module xxHIT.
C.   The current material parameters are stored in /GCMATE/
C.   Then in case of production of secondaries during the last tracking
C.   step , control is transferred to ASKTRK to store the secondaries
C.   either in the standard KINEmatics banks or in a temporary STACK.
C. - Called from    GTVOL, GTRACK                        from GEANT3 pam
C. - Calls          GSXYZ, GPCXYZ                        from GEANT3 pam
C.                  VDHIT, ITHIT, TPHIT, ECHIT, HCHIT    from this .HLB
C.                  MDHIT, LCHIT, SAHIT, VDHITE          from this .HLB
C.                  ASKTRK                               from this .HLB
C.                  UCOPY                                from CERN lib
C.                  CHAINT                     from BOS77.hlb
C.
C --------------------------------------------------------
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
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      PARAMETER (LGKINE=100)
      COMMON/GCKING/KGCASE,NGKINE,GKIN(5,LGKINE),GTOFD(LGKINE)
     &             ,IGFLGK(LGKINE)
C
      COMMON/GCKIN3/GPOS(3,LGKINE)
      REAL GPOS
C
      COMMON/GCNUM/NGMATE,NGVOLU,NGROTM,NGTMED,NGTMUL,NGTRAC,NGPART
     +     ,NGSTMA,NGVERT,NGHEAD,NGBIT
      COMMON/GCNUMX/ NGALIV,NGTMST
C
      COMMON/GCMATE/NGMAT,NGAMAT(5),GA,GZ,GDENS,GRADL,GABSL
C
      LOGICAL FCALO
      COMMON/GCKINE/IGKINE,GPKINE(10),IGTRA,IGSTAK,IGVERT,IGPART,IGTRTY
     +              ,NGAPAR(5),GMASS,GCHARG,GTLIFE,GVERT(3),GPVERT(4)
     +              ,IGPAOL
C
      COMMON/GCTMED/NGUMED,NGATME(5),IGSVOL,IGFIEL,GFIELD,GTMXFD
     +      ,GDMXMS,GDEEMX,GEPSIL,GSTMIN,GCFIEL,GCMUL
     +      ,IGUPD,IGSTPA,NGUMOL
C
      COMMON/GCTRAK/GVECT(7),GETOT,GEKIN,GVOUT(7),NGMEC,LGMEC(30)
     &,NGAMEC(30),NGSTEP,MGXNST,GDESTP,GDESTL,GSAFET,GSLENG,GSTEP,GSNEXT
     &,GSFIEL,GTOFG,GEKRAT,GUPWGH,IGNEXT,IGNWVO,IGSTOP,IGAUTO,IGEKBI
     &,IGLOSL,IGMULL,IGNGOT,NGLDOW,NGLVIN,NGLVSA,IGSTRY
C
      COMMON /GCVOLU/ NGLEVE,NGAMES(15),NGUMBR(15),
     &  LGVOLU(15),LGINDX(15),IGNFRO,NGLVMX,NGLDEV(15),LGINMX(15),
     &  GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      CHARACTER*4 CHAHOL
      EXTERNAL CHAHOL
C - STOP flag for particles which create a shower
      PARAMETER (NOMOR = 3)
      LOGICAL FIRST,FINTO,FSENS,FGAMS,FCHRG,FTINO
C - 1st point in the volume
      FIRST = IGNWVO.EQ.1
C - point is inside the volume
      FINTO = IGNWVO.EQ.0 .OR. IGNWVO.EQ.2
C - it is a charge particle
      FCHRG = GCHARG.NE.0.
C - the particle is a geantino
      FTINO = IGTRTY.EQ.6
C - photon which stops loosing all its energy
      FGAMS = IGTRTY.EQ.1 .AND. NGKINE.LT.2 .AND. IGSTOP.NE.0
C - the point is in a sensitive region
      FSENS = IGSVOL.GT.0
C
C - particle which stops with a hadron mechanism
      FTRHAD = IGSTOP.NE.0 .AND. NGKINE.GT.1
     &        .AND. CHAHOL(KGCASE).EQ.'HADR'
     &        .AND. (IGTRTY.EQ.3 .OR. IGTRTY.EQ.4)
C ----------------------------------------------------------------------
C
C - IF the current particle stops for any reason other than
C   lost of its identy or energy below cut THEN  RETURN
C
      IF (IGSTOP.GE.NOMOR .OR. IGNWVO.EQ.3) RETURN
C
C - volume name
      TRKVOL = CHAHOL (NGAMES(NGLEVE))
C - definition of a calorimeter region where shower can be created
      FCALO = (TRKVOL(1:1).EQ.'C' .OR. TRKVOL(1:1).EQ.'E'
     &          .OR. TRKVOL(1:1).EQ.'H') .AND. (TRKVOL.NE.'CDET')
C
C - IF it is a geantino THEN
C      IF 1st entry (bank does not exist) THEN
C         book the bank
C         create shower parameters
C         store parameters in bank 'CAPA',NR=IGTRA
C      ELSE (next entry)
C         get bank CAPA,NR=IGTRA
C         store bank content into /CAHTRA/
C      ENDIF
C   ENDIF
C
      IF (FTINO) THEN
         CALL CAPGET
      ENDIF
C
C - IF NOT in a calorimeter region THEN
C      IF it is a charged particle in a sensitive region THEN
C         fill the McTrackElement in /TRKCOM/
C         and call the right detector
C         and set the stop flag on return from detector module
C
      IF (.NOT. FCALO) THEN
         IF (FCHRG .AND. FSENS) THEN
C - If it is the 1st point ( IGNWVO=1 ) fill ITRKEL and part of TRKELE
C
         IF (FIRST) THEN
            ITRKEL(1) = IGTRA
            ITRKEL(2) = IGSTAK
            ITRKEL(3) = IGVERT
            ITRKEL(4) = IGPART
            ITRKEL(5) = NGAMES(NGLEVE)
            ITRKEL(6) = NGUMBR(NGLEVE)
            ITRKEL(7) = IGSVOL
            ITRKEL(8) = IGNWVO
            ITRKEL(9) = IGSTOP
            ISEN = IUCOMP(ITRKEL(5),IAGSEN(1,1),LSENV)
            IF (ISEN .EQ. 0) THEN
               ITRKEL(10) = 0
            ELSE
               ITRKEL(10)= NGUMBR(IAGSEN(ISEN,2))
            ENDIF
            ITRKEL(11)= IGTRTY
            CALL UCOPY (GVECT,TRKELE,7)
            TRKELE(8) = GETOT
            TRKELE(9) = GSLENG
            TRKELE(10)= GTOFG
            TRKELE(11)= GSTEP
            TRKELE(12)= GDESTP
            TRKELE(13)= GMASS
            TRKELE(14)= GCHARG
            TRKELE(15)= GTRAN(1,NGLEVE)
            TRKELE(16)= GTRAN(2,NGLEVE)
            TRKELE(17)= GTRAN(3,NGLEVE)
            CALL UCOPY (GRMAT(1,NGLEVE),TRKELE(18),10)
         ENDIF
C
C - If the point is inside the medium ( IGNWVO=0,2 ) complete TRKELE
C   and fill TRKNXT
C
         IF (FINTO) THEN
            IF (ITRKEL(8).EQ.0 .AND. .NOT.FIRST) THEN
               CALL UCOPY (TRKNXT(1),TRKELE(1),10)
            ENDIF
            CALL UCOPY (GVECT(1),TRKNXT(1),7)
            TRKNXT(8) = GETOT
            TRKNXT(9) = GSLENG
            TRKNXT(10)= GTOFG
C
            TRKELE(11) = GSTEP
            TRKELE(12) = GDESTP
            ITRKEL(8)  = IGNWVO
            ITRKEL(9)  = IGSTOP
         ENDIF
C
            IF(IGSVOL.EQ.IDETJO(1)) THEN
               CALL VDHIT
            ELSE IF(IGSVOL.EQ.IDETJO(2) ) THEN
               CALL ITHIT
            ELSE IF(IGSVOL.EQ.IDETJO(3) ) THEN
               CALL TPHIT
            ELSE IF(IGSVOL.EQ.IDETJO(5)) THEN
               CALL LCHIT
            ELSE IF(IGSVOL.EQ.IDETJO(9)) THEN
               CALL SIHIT
            ELSE IF(IGSVOL.EQ.IDETJO(8)) THEN
               CALL MUHIT
            ELSE IF(IGSVOL.EQ.IDETJO(6)) THEN
               CALL SAHIT
            ENDIF
            IGSTOP = ITRKEL(9)
         ENDIF
C
C - ELSE IF it is in a calo. sensitive region OR in the HCAL   AND
C           it is a charge particle OR a photon loosing its energy OR
C           it is a hadron stopping by a HADRon mechanism OR
C           it is a geantino THEN
C       fill the McTrackElement in /TRKCOM/
C       and call the current detector
C       and set the stop flag on return from detector module
C
       ELSEIF ((FSENS .OR. (TRKVOL(1:1).EQ.'H'))
     +  .AND. (FCHRG.OR.FGAMS.OR.FTRHAD.OR.FTINO)) THEN
         IF (FGAMS .OR. (.NOT.FCHRG .AND. FTRHAD)) FIRST=.TRUE.
C - If it is the 1st point ( IGNWVO=1 ) fill ITRKEL and part of TRKELE
C
         IF (FIRST) THEN
            ITRKEL(1) = IGTRA
            ITRKEL(2) = IGSTAK
            ITRKEL(3) = IGVERT
            ITRKEL(4) = IGPART
            ITRKEL(5) = NGAMES(NGLEVE)
            ITRKEL(6) = NGUMBR(NGLEVE)
            ITRKEL(7) = IGSVOL
            ITRKEL(8) = IGNWVO
            ITRKEL(9) = IGSTOP
            ISEN = IUCOMP(ITRKEL(5),IAGSEN(1,1),LSENV)
            IF (ISEN .EQ. 0) THEN
               ITRKEL(10) = 0
            ELSE
               ITRKEL(10)= NGUMBR(IAGSEN(ISEN,2))
            ENDIF
            ITRKEL(11)= IGTRTY
            CALL UCOPY (GVECT,TRKELE,7)
            TRKELE(8) = GETOT
            TRKELE(9) = GSLENG
            TRKELE(10)= GTOFG
            TRKELE(11)= GSTEP
            TRKELE(12)= GDESTP
            TRKELE(13)= GMASS
            TRKELE(14)= GCHARG
            TRKELE(15)= GTRAN(1,NGLEVE)
            TRKELE(16)= GTRAN(2,NGLEVE)
            TRKELE(17)= GTRAN(3,NGLEVE)
            CALL UCOPY (GRMAT(1,NGLEVE),TRKELE(18),10)
         ENDIF
C
C - If the point is inside the medium ( IGNWVO=0,2 ) complete TRKELE
C   and fill TRKNXT
C
         IF (FINTO) THEN
            IF (ITRKEL(8).EQ.0 .AND. .NOT.FIRST) THEN
               CALL UCOPY (TRKNXT(1),TRKELE(1),10)
            ENDIF
            CALL UCOPY (GVECT(1),TRKNXT(1),7)
            TRKNXT(8) = GETOT
            TRKNXT(9) = GSLENG
            TRKNXT(10)= GTOFG
C
            TRKELE(11) = GSTEP
            TRKELE(12) = GDESTP
            ITRKEL(8)  = IGNWVO
            ITRKEL(9)  = IGSTOP
         ENDIF
C
         IF (TRKVOL(1:1).EQ.'E') THEN
            IF (FIRST) THEN
               IF(CHAHOL(NGAMES(4)).EQ.'ECMB') THEN
                  ITRKEL(10)=8-NGUMBR(4)+(NGUMBR(4)/8)*12
               ENDIF
            ENDIF
            CALL ECHIT
         ELSE
            IF (FIRST) THEN
               IF(TRKVOL(2:2).EQ.'B') THEN
                  ITRKEL(10) = 2*(NGUMBR(4)-1) + NGUMBR(5)-1
                  IF (ITRKEL(10).EQ.0) ITRKEL(10) = 24
               ELSE
                  IF(CHAHOL(NGAMES(4)).EQ.'HCMB') THEN
                     ITRKEL(10)=4-NGUMBR(4)+(NGUMBR(4)/4)*6
                  ENDIF
               ENDIF
            ENDIF
            CALL HCHIT
         ENDIF
         IGSTOP = ITRKEL(9)
C
C - ELSE IF it is in a cal. non sensitive region AND geantino param.
C    AND it is a hadron stopping by a HADRon mechanism
C    fill the McTrackElement in /TRKCOM/
C    and call CALHIT
C
      ELSE IF (.NOT.FSENS .AND. FPARJO .AND. FTRHAD) THEN
         FIRST = .TRUE.
C - If it is the 1st point ( IGNWVO=1 ) fill ITRKEL and part of TRKELE
C
         IF (FIRST) THEN
            ITRKEL(1) = IGTRA
            ITRKEL(2) = IGSTAK
            ITRKEL(3) = IGVERT
            ITRKEL(4) = IGPART
            ITRKEL(5) = NGAMES(NGLEVE)
            ITRKEL(6) = NGUMBR(NGLEVE)
            ITRKEL(7) = IGSVOL
            ITRKEL(8) = IGNWVO
            ITRKEL(9) = IGSTOP
            ISEN = IUCOMP(ITRKEL(5),IAGSEN(1,1),LSENV)
            IF (ISEN .EQ. 0) THEN
               ITRKEL(10) = 0
            ELSE
               ITRKEL(10)= NGUMBR(IAGSEN(ISEN,2))
            ENDIF
            ITRKEL(11)= IGTRTY
            CALL UCOPY (GVECT,TRKELE,7)
            TRKELE(8) = GETOT
            TRKELE(9) = GSLENG
            TRKELE(10)= GTOFG
            TRKELE(11)= GSTEP
            TRKELE(12)= GDESTP
            TRKELE(13)= GMASS
            TRKELE(14)= GCHARG
            TRKELE(15)= GTRAN(1,NGLEVE)
            TRKELE(16)= GTRAN(2,NGLEVE)
            TRKELE(17)= GTRAN(3,NGLEVE)
            CALL UCOPY (GRMAT(1,NGLEVE),TRKELE(18),10)
         ENDIF
C
C - If the point is inside the medium ( IGNWVO=0,2 ) complete TRKELE
C   and fill TRKNXT
C
         IF (FINTO) THEN
            IF (ITRKEL(8).EQ.0 .AND. .NOT.FIRST) THEN
               CALL UCOPY (TRKNXT(1),TRKELE(1),10)
            ENDIF
            CALL UCOPY (GVECT(1),TRKNXT(1),7)
            TRKNXT(8) = GETOT
            TRKNXT(9) = GSLENG
            TRKNXT(10)= GTOFG
C
            TRKELE(11) = GSTEP
            TRKELE(12) = GDESTP
            ITRKEL(8)  = IGNWVO
            ITRKEL(9)  = IGSTOP
         ENDIF
C
         CALL CALHIT
         IGSTOP = ITRKEL(9)
      ENDIF
C
C - stop neutrons below 50.Mev
      IF (IGPART.EQ.13 .AND. GVECT(7).LT.0.050) IGSTOP = NOMOR
C
C - stop particles after 20 meters of tracking
      IF (GSLENG .GT. 2000.) IGSTOP = NOMOR
C - Keep secondaries
      IF( (FCHRG .AND. IGSTOP.NE.0 .AND. IGSTAK.EQ.0) .OR.
     &    (NGKINE.GT.0) ) THEN
         CALL ASKTRK
      ENDIF
C
C mark the end of a vdet track segment
C
      IF (FCHRG.AND.TRKVOL(1:1).EQ.'V'.AND.
     &    ((.NOT.FSENS).OR.IGSTOP.EQ.NOMOR)) CALL VDHITE

C
C - this is the end of the normal GUSTEP
C   the next routine ASTDEB is called in case of specific
C   work to be done by GALEPH : special output, debug, history
C   FGALJO is set in ASIJOB after reading of data cards
C
       IF (FGALJO) CALL ASTDEB
C
      GOTO 999
C
 999  CONTINUE
      END
