*DK galeph
      PROGRAM GALEPH
C ----------------------------------------------------------------------
C! MonteCarlo main routine
C. - F.RANJARD - 850222
*CD version
C - GALEPH   30.3   950821  18:17:44
      PARAMETER (GALVER=30.3)
      PARAMETER (CORVER=2.0)
C

C ---------------------------------------------------------------------
      PARAMETER (LGB = 700000, LHB = 20000)
      COMMON /PAWC/   HB(LHB)
      COMMON /GCBANK/   GB(LGB)
C
C
C ----------------------------------------------------------------------
C - call TIMEST to initialize job time limit , MANDATORY on VAX when
C   running interactively, dummy routine on other machines and in BATCH
      CALL TIMEST (9999999.)
C
C - Initialize blank common for HBOOK and /GCBANK/ for GEANT3
C
      CALL GZEBRA (LGB)
      CALL HLIMIT (-LHB)
C
C - GALEPH initialization
C
      CALL ASIGAL
C
C - Process the run : loop over events
C   Then close the run and the job if time limit or last event
C   or end of run or end of file
C
      CALL QNEXTE
C
      STOP
      END
*DK asinit
      SUBROUTINE ASINIT
C ----------------------------------------------------------------------
C. - F.RANJARD - 850215
C! Initialize general constants
C
C - modified by : F.Ranjard - 911002
C                 add SICAL in detector list as # 9
C                 suppress TS from component list
C                 move COIL from #9 to #13 in component list
C                 add references to SICAL
C - Called from   ASIJOB                         from this .HLB
C - Calls         NAMIND, BKFMT                  from BOS77.lib
C.
C -------------------------------------------------------------------
*CD version
C - GALEPH   30.3   950821  18:17:44
      PARAMETER (GALVER=30.3)
      PARAMETER (CORVER=2.0)
C

*CD jobcom
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
*CD joberr
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      INTEGER ITELJO, KERRJO, NERRJO
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C





*CD iocom
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C



*CD alfgeo
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C

*CD jqcom
*CD bcs
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


*CD kincom
      COMMON /KINCOM/   IPROKI,ECMSKI,IDEVKI,ISTAKI,WEITKI
     &                 ,NOTRKI,NITRKI,NIVXKI
C


      EXTERNAL NAMIND
      CHARACTER TITL*40
      CHARACTER*2 TGEO(LBGE)
      DATA TGEO /'VD','IT','TP','EC','LC','SA','HC','MU','SI'
     1          ,'BP','QU','PM','CO','EB','HB','MB','MM','MC'/
      DATA TITL /' GALEPH : THE ALEPH SIMULATION PROGRAM  '/
C ----------------------------------------------------------------------
C
C                   /JOBERR/
      CALL ALTELL ('ASINIT: ',0,'INIT')
C
C -           /IOCOM/
      LGETIO = 1
      LSAVIO = 2
      LGRAIO = 3
      LRDBIO = 4
      LINPIO = IW(5)
      LOUTIO = IW(6)
      DO 1 I=1,LFIL
      TFILIO(I) = ' '
      TFORIO(I) = ' '
 1    CONTINUE
C
C -           /JOBCOM/
      TITLJO=TITL
      VERSJO = GALVER
      IEV1JO = 1
      IEV2JO = 100000
      IEV3JO = 0
      IDB1JO = 0
      IDB2JO = 0
      IDS1JO = 0
      IDS2JO = 0
      MHISJO = 0
      CALL VZERO (FHISJO,LHIS)
      CALL VZERO (IRNDJO,LPRO*LRND)
      CALL VZERO (NRNDJO,LRND)
      CALL VZERO (IPRIJO,LPRI)
      MSETJO = 0
      CALL VZERO (IDETJO,LDET)
      MSAVJO = 0
      MGETJO = 0
      MPROJO =0
      DO 2 I=1,LPRO
         IPROJO(I) =0
 2    CONTINUE
      IPACJO = 0
      FGALJO = .TRUE.
      FXXXJO = .FALSE.
      FWRDJO = .FALSE.
      FXTKJO = .TRUE.
      FXSHJO = .TRUE.
      CUTFJO = 0.1
      TVERJO = 0.
      NEVTJO=0
      NZBOJO = 0
C                      bin size for total # of tracks / event
      MBINJO(1) = 150
C                      bin size for long life tracks / event
      MBINJO(2) = 10
C                      bin size for max. stack size / event
      MBINJO(3) = 5
      CALL VZERO (NSTAJO,LST1*LST2)
C
C - Tracking momentum cut for gammas/elec/charged had/neut.had/muon
      TCUTJO(1) = 0.005
      TCUTJO(2) = 0.005
      TCUTJO(3) = 0.01
      TCUTJO(4) = 0.01
      TCUTJO(5) = 0.01
C
C - 2 letter-codes of various components
C - Initialize Bremstrahlung package
      IBREJO = 1
C
      DO 3 I=1,LBGE
 3    TGEOJO(I) = TGEO(I)
C
C - geometry level
      DO 4 I=1,LGEO
 4    LVELJO(I) = 1
C      set ECAL geometry level to 2 by default  (3 stacks)
      LVELJO(4) = 2
C
C - run condition detector flags: flags are set to the 'STANDARD' value
      CALL VZERO (ICVDJO,LCVD)
      CALL VZERO (ICITJO,LCIT)
C
C  Set run conditions flags to standard values
C  (these values can be overwritten via the run card RUNC 'TPC' ...)
C  for the time being (april 86) only level 1 simulation for pads and
C  trigger pads is avalaible (ICTPJO(2) = ICTPJO(3) = 1)
C  track element banks 'TPTE' and 'TPHE' are not kept (ICTPJO(4)=0)
C  Fill the TPCO bank by default (ICTPJO(6)=1), this bank will be
C  drop by TPCSIM
C
      CALL VZERO (ICTPJO,LCTP)
      ICTPJO(2) = 1
      ICTPJO(3) = 1
      ICTPJO(6) = 1
C
      CALL VZERO (ICECJO,LCEC)
C -   geantino parametrization by default (ICECJO(5)=2)
C     elec parametrization can be turn on with ICECJO(5)=1
      ICECJO(5) = 2
      CALL VZERO (ICLCJO,LCLC)
      CALL VZERO (ICSAJO,LCSA)
      CALL VZERO (ICHCJO,LCHC)
      ICHCJO(2) = 1
      ICHCJO(6) = 1
      CALL VZERO (ICMUJO,LCMU)
      CALL VZERO (ICSIJO,LCSI)
C
C -                   /KINCOM/
      IPROKI = 0
      IDEVKI=0
      ECMSKI=0.
      NOTRKI = 100
      NIVXKI = 9999
      WEITKI = 1.
C
C - Kinematic parameters
C                        kinematyc type
      TKINJO = 'LUND'
C                        vertex sigma x,y,z
      NKINJO = 0
      BKINJO(1) = 0.
      BKINJO(2) = 0.
      BKINJO(3) = 0.
C                         beam energy in center of mass
      BKINJO(4) = 0.
C
C -             /ALFGEO/
      ALRMAX=650.
      ALZMAX=600.
      ALFIEL = 15.
      ALECMS = 90.
C
C - set run number to 1
      IRUNJO = 1
      TRUNJO = ' '
C - set time required to finish the job to 0.
C - date and time
      CALL DATIME (JDATJO,JTIMJO)
C - set the date of the chosen survey file to the 1st January
C   of the current year.
      IDATJO = JDATJO/10000 * 100 + 1
C
C - Get name-index
C
      NARUNH = NAMIND ('RUNH')
      NAPART = NAMIND ('PART')
      NAVERT = NAMIND ('VERT')
      NAKINE = NAMIND ('KINE')
      NAKRUN = NAMIND ('KRUN')
      NAKEVH = NAMIND ('KEVH')
      NAEVEH = NAMIND ('EVEH')
      NAIMPA = NAMIND ('IMPA')
      NARUNR = NAMIND ('RUNR')
      NAASEV = NAMIND ('ASEV')
      NARUNE = NAMIND ('RUNE')
      NAKLIN = NAMIND ('KLIN')
      NAKVOL = NAMIND ('KVOL')
      NAVOLU = NAMIND ('VOLU')
C
C - set bank formats
C
      CALL BKFMT ('ASEV','(I)')
      CALL BKFMT ('VERT','3I,4F,(I)')
      CALL BKFMT ('KINE','3I,4F,(I)')
      CALL BKFMT ('IMPA','2I,(A,7F)')
      CALL BKFMT ('KVOL','2I,(A)')
      CALL BKFMT ('VOLU','2I,(A)')
C
C - write the title
C
      WRITE(LOUTIO,801) TITLJO,VERSJO,CORVER,JDATJO,JTIMJO
 801  FORMAT(1H1,//10X,A40,3X,'VERSION ',F7.2,2X,'CORRECTIONS ',
     &       F7.2,3X,'DATE',I8,2X,'TIME',I5/)
      CALL ALVERS (AVERJO)
C
       END
*DK asrust
      SUBROUTINE ASRUST
C ----------------------------------------------------------------------
C. - F.RANJARD - 850325
C! Build the galeph run header banks
C    ATIT,ACUT,AFID,ARUN,APRO,AJOB,AKIN,ASIM
C
C - modified by : F.Ranjard -911002
C                 modified word(JAJOSD) in AJOB bank
C                 the word contains 1 bit per detector starting
C                 at bit 0. The bit is set to 1 if the detector
C                 was selected on the SETS data card.
C                 add SICAL run conditions in ARUN bank
C
C. - called by    ASIRUN                           from this .HLB
C. - Calls        BLIST                            from BOS.HLB
C                 UCOPY, BUNCH                     from CERNlibs
C.
C -----------------------------------------------------
      SAVE
*CD gcbank
      PARAMETER (KGWBK=69000,KGWRK=5200)
      COMMON /GCBANK/   NGZEBR,GVERSN,GZVERS,IGXSTO,IGXDIV,IGXCON,
     &                  GFENDQ(16),LGMAIN,LGR1,GWS(KGWBK)
      DIMENSION IGB(1),GB(1),LGB(8000),IGWS(1)
      EQUIVALENCE (GB(1),IGB(1),LGB(9)),(LGB(1),LGMAIN),(IGWS(1),GWS(1))
C
*CD gclink
      COMMON/GCLINK/JGDIGI,JGDRAW,JGHEAD,JGHITS,JGKINE,JGMATE,JGPART
     +        ,JGROTM,JGRUN,JGSET,JGSTAK,JGGSTA,JGTMED,JGTRAC,JGVERT
     +        ,JGVOLU,JGXYZ,JGPAR,JGPAR2,JGSKLT
C


*CD version
C - GALEPH   30.3   950821  18:17:44
      PARAMETER (GALVER=30.3)
      PARAMETER (CORVER=2.0)
C

*CD iocom
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C



*CD jobcom
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
*CD joberr
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      INTEGER ITELJO, KERRJO, NERRJO
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C





*CD bcs
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C

*CD ajjpar
*CD acutjj
      INTEGER JACUMN,JACUGC,JACUEC,JACUHC,JACUNC,JACUMC,LACUTA
      PARAMETER(JACUMN=1,JACUGC=2,JACUEC=3,JACUHC=4,JACUNC=5,JACUMC=6,
     +          LACUTA=6)

*CD afidjj
      INTEGER JAFIAR,JAFIAZ,JAFIMF,JAFIBE,LAFIDA
      PARAMETER(JAFIAR=1,JAFIAZ=2,JAFIMF=3,JAFIBE=4,LAFIDA=4)

*CD ajobjj
      INTEGER JAJOBM,JAJORM,JAJOGI,JAJOSO,JAJOSD,JAJOGC,JAJOJD,JAJOJT,
     +          JAJOGV,JAJOAV,JAJOFT,JAJOFS,JAJOFC,JAJODV,JAJODD,JAJOTV,
     +          JAJOCV,JAJOGN,LAJOBA
      PARAMETER(JAJOBM=1,JAJORM=2,JAJOGI=3,JAJOSO=4,JAJOSD=5,JAJOGC=6,
     +          JAJOJD=7,JAJOJT=8,JAJOGV=9,JAJOAV=10,JAJOFT=11,
     +          JAJOFS=12,JAJOFC=13,JAJODV=14,JAJODD=15,JAJOTV=16,
     +          JAJOCV=17,JAJOGN=18,LAJOBA=18)

*CD akinjj
      INTEGER JAKIKT,JAKIKP,LAKINA
      PARAMETER(JAKIKT=1,JAKIKP=2,LAKINA=9)

*CD aprojj
      INTEGER JAPRPF,JAPRRG,LAPROA
      PARAMETER(JAPRPF=1,JAPRRG=2,LAPROA=4)

*CD arunjj
      INTEGER JARURC,LARUNA
      PARAMETER(JARURC=1,LARUNA=10)

*CD asevjj
      INTEGER JASERG,LASEVA
      PARAMETER(JASERG=1,LASEVA=3)

*CD atitjj
      INTEGER JATIRT,LATITA
      PARAMETER(JATIRT=1,LATITA=1)


*CD asimjj
      INTEGER JASIYM,LASIMA
      PARAMETER(JASIYM=1,LASIMA=1)

*CD alcons
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)

*CD alfgeo
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C

      INTEGER IDET(LDET)
      EXTERNAL NAMIND
C ----------------------------------------------------------------------
C
C - get indices of Axxx banks
      JATIT = IW(NAMIND('ATIT'))
      JAKIN = IW(NAMIND('AKIN'))
      JACUT = IW(NAMIND('ACUT'))
      JAFID = IW(NAMIND('AFID'))
      JAJOB = IW(NAMIND('AJOB'))
      JARUN = IW(NAMIND('ARUN'))
      JAPRO = IW(NAMIND('APRO'))
      CALL BLIST (IW,'C+','ATITAKINACUTAFIDAJOBARUNAPRO')
      CALL BKFMT ('ACUT','2I,(A,5F)')
      CALL BKFMT ('AFID','2I,(F)')
      CALL BKFMT ('AJOB','I')
      CALL BKFMT ('AKIN','2I,A,(F)')
      CALL BKFMT ('APRO','I')
      CALL BKFMT ('ARUN','I')
      CALL BKFMT ('ATIT','2I,(A)')
C
C - Build ASIM with date of the geometry
C
      IF (IW(NAMIND('ASIM')).EQ.0) THEN
         CALL ALBOS ('ASIM',0,LASIMA+LMHLEN,JASIM,IGARB)
         IW(JASIM+LMHCOL) = LASIMA
         IW(JASIM+LMHROW) = 1
         IW(JASIM+LMHLEN+JASIYM) = IDATJO
         CALL BKFMT ('ASIM','I')
      ENDIF
      CALL BLIST (IW,'C+','ASIM')
C
C - Build AJOB with all job flags set by data cards
      CALL ALBOS ('AJOB',0,LAJOBA+LMHLEN,JAJOB,IGARB)
      IW(JAJOB+LMHCOL) = LAJOBA
      IW(JAJOB+LMHROW) = 1
      KAJOB = JAJOB + LMHLEN
      IW(KAJOB+JAJOBM) = IBREJO
      IW(KAJOB+JAJORM) = IPACJO
      IW(KAJOB+JAJOJD) = JDATJO
      IW(KAJOB+JAJOJT) = JTIMJO
      IW(KAJOB+JAJOGV) = NINT(VERSJO*10.)
      IW(KAJOB+JAJOAV) = NINT(AVERJO*10.)
      IW(KAJOB+JAJOGI) = MGETJO
      IW(KAJOB+JAJOSO) = MSAVJO
      DO 10 I=1,LDET
         IDET(I) = 0
         IF (IDETJO(I).NE.0) IDET(I) = 1
 10   CONTINUE
      CALL BUNCH (IDET,IW(KAJOB+JAJOSD),LDET,1)
      CALL BUNCH (IGEOJO,IW(KAJOB+JAJOGC),LGEO,1)
      IF (FXTKJO) IW(KAJOB+JAJOFT) = 1
      IF (FXSHJO) IW(KAJOB+JAJOFS) = 1
      IW(KAJOB+JAJOFC) = NINT(CUTFJO*1000.)
      IW(KAJOB+JAJODV) = IDAFJO
      IW(KAJOB+JAJODD) = IDCHJO
      IW(KAJOB+JAJOTV) = NINT(TVERJO*100.)
      IW(KAJOB+JAJOCV) = NINT(CORVER*100.)
      IW(KAJOB+JAJOGN) = INT(GVERSN*10000.)
C
C - Build  ATIT with the run title
      IF (JATIT .EQ. 0) THEN
         IF (TRUNJO.NE.' ') THEN
            LTIT = (LENOCC(TRUNJO)+3)/4
            CALL ALBOS ('ATIT',0,LTIT+LMHLEN,JATIT,IGARB)
            IW(JATIT+LMHCOL) = 1
            IW(JATIT+LMHROW) = LTIT
            KATIT = JATIT + LMHLEN
            CALL ALINST(TRUNJO,IW(KATIT+JATIRT),LTIT)
         ENDIF
      ENDIF
C
C - Build  ACUT with the tracking cuts
      CALL ALBOS ('ACUT',0,LACUTA+LMHLEN,JACUT,IGARB)
      IW(JACUT+LMHCOL) = LACUTA
      IW(JACUT+LMHROW) = 1
      KACUT = JACUT + LMHLEN
      IW(KACUT+JACUMN) = INTCHA ('ALEF')
      CALL UCOPY (TCUTJO,RW(KACUT+JACUGC),LTCUT)
C
C - Build AFID with ALFGEO common variables
      CALL ALBOS ('AFID',0,LAFIDA+LMHLEN,JAFID,IGARB)
      IW(JAFID+LMHCOL) = LAFIDA
      IW(JAFID+LMHROW) = 1
      KAFID = JAFID + LMHLEN
      RW(KAFID+JAFIAR) = ALRMAX
      RW(KAFID+JAFIAZ) = ALZMAX
      RW(KAFID+JAFIMF) = ALFIEL
      RW(KAFID+JAFIBE) = ALECMS
C
C - Build APRO with process flags and random generator rootsk
      CALL ALBOS ('APRO',0,LAPROA*LPRO+LMHLEN,JAPRO,IGARB)
      IW(JAPRO+LMHCOL) = LAPROA
      IW(JAPRO+LMHROW) = LPRO
      KAPRO = JAPRO + LMHLEN
      DO 1 I=1,LPRO
         IW(KAPRO+JAPRPF) = IPROJO(I)
         IW(KAPRO+JAPRRG) = IRNDJO(1,I)
         IW(KAPRO+JAPRRG+1) = IRNDJO(2,I)
         IW(KAPRO+JAPRRG+2) = IRNDJO(3,I)
  1   KAPRO = KAPRO + LAPROA
C
C - Build AKIN with the kinematic parameters
      LKIN = MIN (LKINP,NKINJO)
      IF (LKIN .GT. 0) THEN
         IF (JAKIN .EQ. 0) THEN
            CALL ALBOS ('AKIN',0,LKIN+1+LMHLEN,JAKIN,IGARB)
            IW(JAKIN+LMHCOL) = 1
            IW(JAKIN+LMHROW) = LKIN+1
         ENDIF
         KAKIN = JAKIN + LMHLEN
         IW(KAKIN+JAKIKT) = INTCHA (TKINJO)
         CALL UCOPY (BKINJO,RW(KAKIN+JAKIKP),LKIN)
      ENDIF
C
C - Build ARUN with the detector run conditions
      LARUN = MAX (LCVD,LCIT,LCTP,LCEC,LCLC,LCSA,LCHC,LCMU)
      CALL ALBOS ('ARUN',0,LARUN*LDET+LMHLEN,JARUN,IGARB)
      IW(JARUN+LMHCOL) = LARUN
      IW(JARUN+LMHROW) = LDET
      KARUN = JARUN + LMHLEN
      CALL UCOPY (ICVDJO,IW(KARUN+JARURC),LCVD)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICITJO,IW(KARUN+JARURC),LCIT)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICTPJO,IW(KARUN+JARURC),LCTP)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICECJO,IW(KARUN+JARURC),LCEC)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICLCJO,IW(KARUN+JARURC),LCLC)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICSAJO,IW(KARUN+JARURC),LCSA)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICHCJO,IW(KARUN+JARURC),LCHC)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICMUJO,IW(KARUN+JARURC),LCMU)
      KARUN = KARUN + LARUN
      CALL UCOPY (ICSIJO,IW(KARUN+JARURC),LCSI)
C
C - End
 999  CONTINUE
      RETURN
       END
*DK aswsum
      SUBROUTINE ASWSUM
C ----------------------------------------------------------------------
C. - F.RANJARD - 850328    modified - 920131
C! Print statistic for this run
C. - Called from    ASCJOB                            from this .HLB
C. - Calls          TIMAX                             from KERNLIB
C.                  GFTITL                            from GEANT3 pam
C.                  BOSTA                             from BOS77.hlb
C.
C -----------------------------------------------------
      SAVE
*CD version
C - GALEPH   30.3   950821  18:17:44
      PARAMETER (GALVER=30.3)
      PARAMETER (CORVER=2.0)
C

*CD jqcom
*CD bcs
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


*CD iocom
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C



*CD jobcom
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
*CD joberr
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      INTEGER ITELJO, KERRJO, NERRJO
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C





*CD alfgeo
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C

*CD gcbank
      PARAMETER (KGWBK=69000,KGWRK=5200)
      COMMON /GCBANK/   NGZEBR,GVERSN,GZVERS,IGXSTO,IGXDIV,IGXCON,
     &                  GFENDQ(16),LGMAIN,LGR1,GWS(KGWBK)
      DIMENSION IGB(1),GB(1),LGB(8000),IGWS(1)
      EQUIVALENCE (GB(1),IGB(1),LGB(9)),(LGB(1),LGMAIN),(IGWS(1),GWS(1))
C
*CD gclink
      COMMON/GCLINK/JGDIGI,JGDRAW,JGHEAD,JGHITS,JGKINE,JGMATE,JGPART
     +        ,JGROTM,JGRUN,JGSET,JGSTAK,JGGSTA,JGTMED,JGTRAC,JGVERT
     +        ,JGVOLU,JGXYZ,JGPAR,JGPAR2,JGSKLT
C


      CHARACTER*40 TITL(LST2)
      PARAMETER (LPAC=3)
      CHARACTER*8 TPAC(LPAC)
      CHARACTER*2 TNAM(LGEO)
      CHARACTER*4 TGET, TSAV, TJET, TLUND*7
      CHARACTER*4 CHAINT,CTABL
      CHARACTER*6 TGEN(3)
C
      DATA TGEN /'RNDM  ', 'RANECU', 'RANMAR'/
      DATA TLUND /'GUDSCBT'/
      DATA TPAC /'GHEISHA ','TATINA  ','CASCADE '/
      DATA TITL /'TOTAL NUMBER OF TRACKS PER EVENT        '
     1          ,'TOTAL NUMBER OF "PRIMARIES" PER EVENT   '
     2          ,'MAXIMUM STACK SIZE PER EVENT            '/
C
*CD bmacro
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C

C - Lth character element of the NRBOSth row of the bank with index ID
      CTABL(ID,NRBOS,L) = CHAINT(ITABL(ID,NRBOS,L))
C ----------------------------------------------------------------------
C
C - get time elapsed since beginning of job
      CALL TIMAX (TIMEJO(3))
C
C - get number of triggers processed and time per trigger
      TIME = 0.
      IF(NEVTJO.GT.0) TIME = (TIMEJO(3)-TIMEJO(1)-TIMEJO(5))/NEVTJO
C
C - print title
      WRITE(LOUTIO,801) TITLJO,VERSJO,CORVER,JDATJO,JTIMJO
  801 FORMAT(1H1,//10X,A40,2X,'VERSION',F7.2,2X,'CORRECTIONS',F7.2,3X,
     &            'DATE',I8,2X,'TIME',I5//)
      WRITE (LOUTIO,'(3X,''ALEPHLIB '',T15,'':'',F7.2)') AVERJO
      WRITE (LOUTIO,'(3X,''TPCSIM   '',T15,'':'',F7.2)') TVERJO
      WRITE (LOUTIO,'(3X,''GEANT    '',T15,'':'',F7.2)') GVERSN*100.
C
      WRITE (LOUTIO,'(3X,''ADBSCONS '',T15,'':'',I5,I9/)')IDAFJO,
     &       IDCHJO
C
C - print summary of data cards used for this run
      IF(MGETJO.EQ.0) THEN
        TGET = 'none'
      ELSE
        TGET = 'all'
      ENDIF
      IF(MSAVJO.EQ.0) THEN
        TSAV = 'none'
      ELSE
        TSAV = 'all'
      ENDIF
      WRITE(LOUTIO,810) TGET, TSAV
      WRITE(LOUTIO,816) (TPROJO(J),J=1,MPROJO)
  810 FORMAT(3X,'STEERING   : READ     ',A4,4X,'SAVE    ',A4)
  816 FORMAT(3X,'             PROCESS  ',12A8)
C
      IF (MGETJO.NE.0) THEN
        WRITE (LOUTIO,811)
      ELSE
        IF (TKINJO.EQ.'USER') THEN
          WRITE (LOUTIO,806) BKINJO
        ELSE IF (TKINJO.EQ.'LUND') THEN
          WRITE(LOUTIO,812) BKINJO(4)
        ELSE IF (TKINJO.EQ.'JET') THEN
          WRITE(LOUTIO,813) (BKINJO(J),J=4,6)
        ELSE IF (TKINJO.EQ.'PART') THEN
          ITYP=NINT(BKINJO(4))
          JPART = IW(NAPART)
          WRITE(LOUTIO,814) CTABL(JPART,ITYP,2),CTABL(JPART,ITYP,3),
     &                        CTABL(JPART,ITYP,4),(BKINJO(J),J=5,8)
        ELSE IF (TKINJO.EQ.'SJET') THEN
          ITYP = INT (ABS (BKINJO(4))-500.) + 1
          TJET = TLUND(ITYP:ITYP)//'   '
          IF (BKINJO(4) .LT. 0.) TJET = TLUND(ITYP:ITYP)//'bar'
          WRITE (LOUTIO,822) TJET,(BKINJO(J),J=5,8)
        ENDIF
      ENDIF
  806 FORMAT(/3X,'KINEMATICS : USER parameters ',8F10.4)
  811 FORMAT(/3X,'KINEMATICS : was read in ')
  812 FORMAT(/3X,'KINEMATICS : LUND events (beam energy =',F6.1,' Gev)')
  813 FORMAT(/3X,'KINEMATICS : LUND single jet (beam energy ='
     1       ,F6.1,' Gev',3X,'theta =',F6.3,3X,'phi =',F6.3,')')
  814 FORMAT(/3X,'KINEMATICS : single ',3A4,' (momentum range =',2F8.3
     1       ,' Gev',3X,'cos(theta) range =',2F6.3,')'  )
  822 FORMAT (/3X,'KINEMATICS : LUND single jet ',A4,' (energy range =',
     &         2F8.3,' Gev',3X,'cos(theta) range =',2F6.3,')' )
C
      WRITE(LOUTIO,815) (TCUTJO(J),J=1,5)
  815 FORMAT(/3X,'TRACKING   : particles with energy below threshold '
     1                ,'are stopped'
     2                /16X,'electron cut          =',F8.4,' Gev'
     3                /16X,'gamma cut             =',F8.4,' Gev'
     5                /16X,'charged hadron cut    =',F8.4,' Gev'
     4                /16X,'neutral hadron cut    =',F8.4,' Gev'
     6                /16X,'muon cut              =',F8.4,' Gev'
     7                )
      CALL ALSEED (IRGEN,ISD1,ISD2)
      IF (IRGEN .EQ. 3) THEN
        WRITE (LOUTIO,817) TGEN(IRGEN),ISD1,ISD2
      ELSE
        WRITE (LOUTIO,817) TGEN(IRGEN)
      ENDIF
  817 FORMAT(/3X,'RANDOM NUMBER GENERATOR is ',A6:' with lab. seeds ',
     &        2I7)
      IHPAC = MOD (IPACJO,10)
      IF (IHPAC.GT.0 .AND. IHPAC.LE.LPAC) THEN
        WRITE (LOUTIO,818) TPAC(IHPAC)
  818   FORMAT(/3X,'HADRONIC  package is ',A8)
      ELSE
        WRITE(LOUTIO,'(''NO HADRONIC package'')')
      ENDIF
      ITPAC = MOD (IPACJO/10,10)
      IF (ITPAC.EQ.1) WRITE (LOUTIO,'(3X,''FAST tracking'')')
      IRPAC = IPACJO/100
      IF (IRPAC.EQ.1) WRITE (LOUTIO,'(3X,''RDST in use'')')
C
      WRITE(LOUTIO,819) ALFIEL
  819 FORMAT(/3X,'MAGNETIC FIELD : nominal value = ',F6.3,' Kgauss')
C
      WRITE(LOUTIO,820) ALRMAX,ALZMAX
  820 FORMAT(/3X,'GEOMETRY   : fiducial volume is RMAX = ',F8.1,' cm',
     1        2X,'and ZMAX = ',F8.1,' cm')
      M=0
      DO 2 L=1,LGEO
        IF(IGEOJO(L).GT.0) THEN
          M=M+1
          TNAM(M)=TGEOJO(L)
        ENDIF
    2 CONTINUE
      WRITE(LOUTIO,821) (TNAM(L),L=1,M)
  821 FORMAT(16X,'components present in the setup are : ',15(A2,2X))
C
C - print statistics for this run
      WRITE(LOUTIO,800) IRUNJO
  800 FORMAT(/3X,'RUN NUMBER',T25,I10)
      WRITE(LOUTIO,802) NEVTJO
  802 FORMAT(3X,'EVENTS ',T25,I10)
      WRITE(LOUTIO,803) TIMEJO(5),TIME
  803 FORMAT(3X,'TIME SPENT',T25,'before 1st event',F8.3,' sec',T50
     1         ,'per event ',F8.3,' sec')
      WRITE(LOUTIO,805) NRNDJO
  805 FORMAT(3X,'LAST RANDOM NUMBER ',T25,3I15)
C
      IF (IPROJO(2).NE.0) THEN
C - print pseudo-histograms on number of tracks / event
        DO 1 I=1,LST2
          WRITE(LOUTIO,807) TITL(I),NSTAJO(LBIN+1,I),LBIN*MBINJO(I)
     1                    ,(NSTAJO(LBIN+L,I),L=2,3)
          WRITE(LOUTIO,808) (N*MBINJO(I),N=1,LBIN),
     &                      (NSTAJO(N,I),N=1,LBIN)
    1   CONTINUE
  807   FORMAT(/3X,A40/3X,'# of events with zero track = ',I3,
     1            5X,'with more than ',I4,' tracks = ',I4,
     2            5X,'max. # of tracks = ',I5)
  808   FORMAT(20I6)
      ENDIF
C
C - print error codes
      WRITE(LOUTIO,804) (N,N=1,LERR),NERRJO
  804 FORMAT(/3X,'ERROR CODES  (definitions in ASCRUN : 804 format)'/
     &        3X,'1. - not enough space to book BOS bank (ALBOS)',
     &        T58,'2. - MUON error : array too small (MUDGTZ)'/
     &        3X,'3. - HCAL error (HCAL module)',
     &        T58,'4. - ECAL error : missing banks (ECAL module)'/
     &        3X,'5. - ITC  error : bank too small (ITDAQ)',
     &        T58,'6. - event did not terminate (ASEVEH)'/
     &        3X,'7. - VDET error',
     &        T58,'8. - not used'/
     &        3X,'9. - LCAL error : wrong data',
     &        T58,'10.- User reject at KINE process'/
     &        3X,'11.- CALO error : missing banks (CALO module)'/
     +        3X,20I5/3X,20I5)
C
C - print BOS statistics
      CALL BOSTA
C
C - print ZEBRA statistics
      CALL MZEND
C
      RETURN
      END
*DK ehsitw
      SUBROUTINE EHSITW(ISCMP,MODUL,TYGEO)
C.----------------------------------------------------------------
C Y.Karyotakis M.Rumpf Dec 85
C! ECAL signals -->addresses
C    Process ECAL Signals for one Geant3 Track Element
C    Find Towers and wire planes addresses
C   - Called by EHTRKE,EHSHOW
C   _ Calls     EHSMTW,EHSMWI,EHTTWD
C.----------------------------------------------------------------
      SAVE
*CD bcs
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C

*CD ecsize
      PARAMETER (LSTCK=3,LPHI=384,LTHET=228)
      PARAMETER (LWPLA=45,LMODU=12,LCOMP=3)
      PARAMETER (NTHSG=12,NPHSG=24)

*CD ecstat
      PARAMETER (NECST = 30)
      COMMON/ ECSTAT / NECONT(NECST),ECCONT(NECST)


*CD ecnamc
      PARAMETER (LPS1=6, LPS2=300)
      COMMON /ECNAMC/   NAETHT, NAEWHT, NAETTD, NAEWTD, NAETDI, NAEWDI
     &                , NAEWHE
     &                , NAETTR, NAEWTR, NAENDI
     &                , IDPSIG, IDEWTM, IDETTM
     &                , NAESHI, NAEWHI
C

*CD trkcom
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C


C

C
      EXTERNAL EFNDCL,EFNDLG
      INTEGER  EFNDCL,EFNDLG
      DIMENSION IPLST(LWPLA)
      CHARACTER * 5 TYGEO
C
*CD bmacro
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C

      DATA NCALL/0/
C
      IF (NCALL.EQ.0)      THEN
        NCALL = 1
        CALL ECPLST(IPLST)
      ENDIF
C
C -------------------------------------------------------------
      IF (IDPSIG.EQ.0) RETURN
C
      NECSG = LROWS(IDPSIG)
C
      IF (NECSG .EQ. 0)                  RETURN
C
C     Loop over all generated Signals
C
      IPX = IDPSIG + LMHLEN
      DO 1 L = 1 , NECSG
C
C     Tranform HIT coordinates to ICL,ILG,STACK for PADS,and WIRE Plane
C
         ICL = EFNDCL(ISCMP,MODUL,RW(IPX+1),TYGEO)
         IF (ICL.LE.0)      THEN
             NECONT(6) = NECONT(6) + 1
             GO TO 1
         ENDIF
         ILG = EFNDLG(ISCMP,MODUL,RW(IPX+1),TYGEO)
         IF (ILG.LE.0)      THEN
            NECONT(7) = NECONT(7) + 1
            GO TO 1
         ENDIF
         CALL EVERIF (ILG,ICL,MODUL,IOKK)
         IF (IOKK.GT.0) ILG = IOKK
C
C probably the previous correction makes this redundant
C
         IF( IW(IPX+6).GT.LWPLA ) IW(IPX+6) = LWPLA
         IST = IPLST( IW(IPX+6) )
         ECCONT(IST) = ECCONT(IST) + IW(IPX+4)
C
C -         Compute tower address and fill 'ESHI'
C
         IADTW = 0
         CALL MVBITS(ICL,0,9,IADTW,2)
         CALL MVBITS(ILG,0,9,IADTW,16)
         CALL CAHIST(NAESHI,IADTW,IST,IW(IPX+4))
C
C-          Compute wire address and fill 'EWHI'
C
         IADWI = (ISCMP-1)*LMODU + MODUL
         CALL CAHIST(NAEWHI,IADWI,IW(IPX+6),IW(IPX+5))
C
    1 IPX = IPX + LCOLS(IDPSIG)
C
      RETURN
      END
*DK everif
      SUBROUTINE EVERIF(ILG,ICL,IMODU,IOK)
C --------------------------------------------------
CKEY ECAL
C! check consistency of ecal address
C - MNM - 960404
C - Input   : ILG    / I = ecal row no.
C             ICL    / I = ecal column no.
C             IMODU  / I = ecal module no.
C - Output  : IOK    / I = new row number if .ne. 0
C --------------------------------------------------
      DIMENSION IBZONE(4) , IPZONE (4)
      DATA IBZONE / 8,24,40,50 /
      DATA IPZONE / 8, 16, 24, 32 /
C --------------------------------------------------
C
C     Verify ILG/ICL consistency in ENDCAP
C
      IOK = 0
      ILG0 = ILG
      IDEBUG = 0
      IF (ILG.LT.50.OR.ILG.GT.178) THEN
         IF ( ILG.GT.178) ILG0 = 228-ILG+1
         IMOD = MOD (IMODU-1,12)+1
         IZON =1
         DO IL =1,3
         IF(ILG0.GT.IBZONE(IL)) IZON = IL+1
         ENDDO
         IMOR =((ICL+(0.5*IPZONE(IZON))-1)/IPZONE(IZON))+1
         IMOR0 = IMOR
         IF (IMOR.GT.12) IMOR =1
         IF ( IMOR .NE.IMOD) THEN
           IF (IMOR0.LT.IMOD) THEN
            IOK = ILG0 -1
           ELSE
            IOK = ILG0 +1
           ENDIF
         ENDIF
       ENDIF
       IF ( ILG.NE.ILG0.AND.IOK.NE.0) IOK = 228-IOK +1
       RETURN
       END
