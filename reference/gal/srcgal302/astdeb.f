      SUBROUTINE ASTDEB
C ---------------------------------------------------------
C - F.Ranjard - 870813
C! debug after a tracking step
C  this routine is called by GUSTEP after every step when
C  FGALJO is true to perform various tasks such as:
C  - IMPAct banks if required
C  - large debug printout if required
C  - specific output to retreive energy deposited in ALEPH
C - called by GUSTEP                         from this .HLB
C - calls     ASTIMP                         from this .HLB
C             GPCXYZ, GSXYZ, GDCXYZ          from GEANT3 lib
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
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
      LOGICAL FTPC,FLAST
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
C
C ----------------------- GALEPH IMPA banks
      IF (NAGIMP.GT.0) THEN
C
C - IF it is a primary track THEN
C      IF it is the entrance point in a detector but TPC gas OR
C         it is the exit point of a charged part. from the TPC gas THEN
C         store the track parameters in bank IMPA, NR=IGTRA
         FTPC  = IGSVOL.GT.0 .AND. IGSVOL.EQ.IDETJO(3)
         FLAST = IGNWVO.EQ.2
         IF (IGSTAK .EQ. 0) THEN
            IF ( (FIRST .AND. .NOT.FTPC) .OR.
     &           (FLAST .AND. FCHRG .AND. FTPC) ) THEN
               CALL ASTIMP
            ENDIF
         ENDIF
C
      ENDIF
C ----------------------- end GALEPH IMPA
C
C - debug
      IF(FDEBJO.AND.IPRIJO(17).EQ.1) THEN
       CALL GPCXYZ
       IF(NGKINE.GT.0) THEN
        WRITE(LOUTIO,801)NGLEVE,(CHAHOL(NGAMES(I)),NGUMBR(I),I=1,NGLEVE)
        WRITE(LOUTIO,802) CHAHOL(KGCASE),NGKINE,
     &                   ((GKIN(J,I),J=1,5),I=1,NGKINE)
       ENDIF
      ENDIF
 801  FORMAT(/1X,'+++ASTDEB+++ ',I2,10(2X,A4,I3))
 802  FORMAT(14X,A4,3X,I3,:,5F10.4,:/(24X,5F10.4))
C
C - If display of the event is required then
C - Save 1st and last point of the track + points when leaving a volume
C - or points every 20cm about
      IF(FDISJO) THEN
         IF(GSLENG.EQ.0. .OR. IGSTOP.NE.0 .OR. IGNWVO.EQ.2
     &      .OR. MOD(GSLENG,20.).EQ.0.) CALL GSXYZ
      ENDIF
C
      END
