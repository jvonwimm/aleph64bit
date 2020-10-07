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
C - GALEPH   30.2   950512  18:17:44
      PARAMETER (GALVER=30.2)
      PARAMETER (CORVER=2.0)
*?          corrections in VDET (MANOJ302)
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
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
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
